--
-- HTTP client for use with io-streams
--
-- Copyright © 2012-2018 Operational Dynamics Consulting, Pty Ltd
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the BSD licence.
--

{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MagicHash          #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_HADDOCK hide, not-home #-}

module Network.Http.Inconvenience (
    URL,
    modifyContextSSL,
    establishConnection,
    get,
    post,
    postForm,
    encodedFormBody,
    put,
    baselineContextSSL,
    concatHandler',
    TooManyRedirects(..),
    HttpClientError(..),

    ConnectionAddress(..),
    connectionAddressFromURI,
    connectionAddressFromURL,
    openConnectionAddress,

        -- for testing
    splitURI
) where

import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as Builder (fromByteString,
                                                      fromWord8, toByteString)
import qualified Blaze.ByteString.Builder.Char8 as Builder (fromString)
import Control.Exception (Exception, bracket, throw)
import Control.Monad (when, unless)
import Data.Bits (Bits (..))
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.ByteString.Internal (c2w, w2c)
import Data.Char (intToDigit, toLower)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (intersperse)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable (Typeable)
import Data.Word (Word16)
import GHC.Exts
import GHC.Word (Word8 (..))
import Network.URI (URI (..), URIAuth (..), isAbsoluteURI,
                    parseRelativeReference, parseURI, uriToString, unEscapeString)
import OpenSSL (withOpenSSL)
import OpenSSL.Session (SSLContext)
import qualified OpenSSL.Session as SSL
import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams
import System.IO.Unsafe (unsafePerformIO)

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (Monoid (..), mappend)
#endif

import Network.Http.Connection
import Network.Http.RequestBuilder
import Network.Http.Types

-- (see also http://downloads.haskell.org/~ghc/8.4.2/docs/html/users_guide/phases.html#standard-cpp-macros
-- for a list of predefined CPP macros provided by GHC and/or Cabal; see also the cabal user's guide)
#if defined(linux_HOST_OS) || defined(freebsd_HOST_OS)
import System.Directory (doesDirectoryExist)
#endif

type URL = ByteString

------------------------------------------------------------------------------

--
-- | URL-escapes a string (see
-- <http://tools.ietf.org/html/rfc2396.html#section-2.4>)
--
urlEncode :: ByteString -> URL
urlEncode = Builder.toByteString . urlEncodeBuilder
{-# INLINE urlEncode #-}


--
-- | URL-escapes a string (see
-- <http://tools.ietf.org/html/rfc2396.html#section-2.4>) into a 'Builder'.
--
urlEncodeBuilder :: ByteString -> Builder
urlEncodeBuilder = go mempty
  where
    go !b !s = maybe b' esc (S.uncons y)
      where
        (x,y)     = S.span (flip Set.member urlEncodeTable) s
        b'        = b `mappend` Builder.fromByteString x
        esc (c,r) = let b'' = if c == ' '
                                then b' `mappend` Builder.fromWord8 (c2w '+')
                                else b' `mappend` hexd c
                    in go b'' r


hexd :: Char -> Builder
hexd c0 = Builder.fromWord8 (c2w '%') `mappend` Builder.fromWord8 hi
                                      `mappend` Builder.fromWord8 low
  where
    !c        = c2w c0
    toDigit   = c2w . intToDigit
    !low      = toDigit $ fromEnum $ c .&. 0xf
    !hi       = toDigit $ (c .&. 0xf0) `shiftr` 4

    shiftr (W8# a#) (I# b#) = I# (word2Int# (uncheckedShiftRL# a# b#))


urlEncodeTable :: Set Char
urlEncodeTable = Set.fromList $! filter f $! map w2c [0..255]
  where
    f c | c >= 'A' && c <= 'Z' = True
        | c >= 'a' && c <= 'z' = True
        | c >= '0' && c <= '9' = True
    f c = c `elem` ("$-_.!~*'(),"::String)


------------------------------------------------------------------------------

{-
    The default SSLContext used by the convenience APIs in the http-io-streams
    library. This is a kludge, unsafe bad yada yada. The technique, however,
    was described on a Haskell Wiki page, so that makes it an officially
    supported kludge. The justification for doing this is a) the functions
    accessing this IORef are themselves all in the IO monad, and b) these
    contortions are necessary to allow the library to be used for https:// URLs
    *without* requiring the developer to do 'withOpenSSL'.
-}
global :: IORef SSLContext
global = unsafePerformIO $ do
    ctx <- baselineContextSSL
    newIORef ctx
{-# NOINLINE global #-}

--
-- | Modify the context being used to configure the SSL tunnel used by
-- the convenience API functions to make @https://@ connections. The
-- default is that setup by 'baselineContextSSL'.
--
modifyContextSSL :: (SSLContext -> IO SSLContext) -> IO ()
modifyContextSSL f = do
    ctx <- readIORef global
    ctx' <- f ctx
    writeIORef global ctx'

--
-- | Given a URL, work out whether it is normal, secure, or unix domain,
-- and then open the connection to the webserver including setting the
-- appropriate default port if one was not specified in the URL. This
-- is what powers the convenience API, but you may find it useful in
-- composing your own similar functions.
--
-- For example (on the assumption that your server behaves when given
-- an absolute URI as the request path), this will open a connection
-- to server @www.example.com@ port @443@ and request @/photo.jpg@:
--
-- >     let url = "https://www.example.com/photo.jpg"
-- >
-- >     c <- establishConnection url
-- >     let q = buildRequest1 $ do
-- >                 http GET url
-- >     ...
--
establishConnection :: URL -> IO (Connection)
establishConnection r' = do
    establish u
  where
    u = parseURL r'
{-# INLINE establishConnection #-}

establish :: URI -> IO (Connection)
establish u =
    case scheme of
        "http:"  -> do
                        openConnection host port
        "https:" -> do
                        ctx <- readIORef global
                        openConnectionSSL ctx host ports
        "unix:"  -> do
                        openConnectionUnix $ uriPath u
        _        -> error ("Unknown URI scheme " ++ scheme)
  where
    scheme = uriScheme u

    auth = case uriAuthority u of
        Just x  -> x
        Nothing -> URIAuth "" "localhost" ""

    host = S.pack (uriRegName auth)
    port = case uriPort auth of
        ""  -> 80
        _   -> read $ tail $ uriPort auth :: Word16
    ports = case uriPort auth of
        ""  -> 443
        _   -> read $ tail $ uriPort auth :: Word16


-- | HTTP connection target
--
-- See also 'connectionAddressFromURL' and 'connectionAddressFromURI'.
--
-- @since 0.1.1.0
data ConnectionAddress
  = ConnectionAddressHttp     !Hostname !Word16 -- ^ Represents @http:\/\/host:port@
  | ConnectionAddressHttps    !Hostname !Word16 -- ^ Represents @https:\/\/host:port@
  | ConnectionAddressHttpUnix FilePath -- ^ Represents @http+unix:\/\/%2Fsome%2Fpath%2Fsocket@ or @unix:\/some\/path\/socket@
  deriving Show

-- | Open a 'Connection' to the specified 'ConnectionAddress'
--
-- This is a simple wrapper over 'openConnection', 'openConnectionSSL', and 'openConnectionUnix'.
--
-- A subtle difference between @'openConnectionUnix' fp@ and @'openConnectionAddress' ('ConnectionAddressHttpUnix' fp)@ is that the latter sends an empty (but valid) @Host:@ HTTP header by default (unless overriden by 'setHostname').
--
-- This operation is often used in combination with 'withConnection'.
--
-- @since 0.1.1.0
openConnectionAddress :: ConnectionAddress -> IO Connection
openConnectionAddress ca = case ca of
  ConnectionAddressHttp  host port  -> openConnection host port
  ConnectionAddressHttps host ports -> do
    ctx <- readIORef global
    openConnectionSSL ctx host ports
  ConnectionAddressHttpUnix fp -> do
    c <- openConnectionUnix fp
    return c { cHost = mempty } -- NB: HTTP RFC allows empty Host: headers

-- | Decode 'URL' into 'ConnectionAddress' and URL path.
--
-- The 'URL' argument is expected to be properly escaped according to RFC 3986.
--
-- This is a wrapper over 'connectionAddressFromURI'
--
-- @since 0.1.1.0
connectionAddressFromURL :: URL -> Either String (ConnectionAddress, ByteString)
connectionAddressFromURL r' = do
  r <- either (\_ -> Left "invalid UTF-8 encoding") return (T.decodeUtf8' r')
  u <- maybe (Left "invalid URI syntax") return (parseURI (T.unpack r))
  connectionAddressFromURI u

-- | Decode 'URI' (from the @network-uri@ package) into 'ConnectionAddress' and (escaped) URL path.
--
-- See also 'connectionAddressFromURL'
--
-- @since 0.1.1.0
connectionAddressFromURI :: URI -> Either String (ConnectionAddress, ByteString)
connectionAddressFromURI u =
    case map toLower (uriScheme u) of
        "http:"      -> Right (ConnectionAddressHttp host (port 80), urlpath)
        "https:"     -> Right (ConnectionAddressHttps host (port 443), urlpath)
        "unix:" -> do
          noPort
          noHost
          when (null (uriPath u)) $
            Left "invalid empty path in URI"
          unless (null (uriQuery u)) $
            Left "invalid query component in URI"
          return (ConnectionAddressHttpUnix (uriPath u), "") -- bug-compatible semantics
        "http+unix:" -> do
          noPort
          fp <- getUnixPath
          return (ConnectionAddressHttpUnix fp, urlpath)
        _ -> Left ("Unknown URI scheme " ++ uriScheme u)
  where
    auth = case uriAuthority u of
        Just x  -> x
        Nothing -> URIAuth "" "localhost" ""

    noPort = if null (uriPort auth) then Right () else Left "invalid port number in URI"
    noHost = case uriAuthority u of
               Nothing -> return ()
               Just (URIAuth "" "" "") -> return ()
               Just _ -> Left "invalid host component in uri"

    getUnixPath = case uriAuthority u of
                   Nothing -> Left "missing host component in uri"
                   Just a
                     | null (uriRegName a) -> Left "missing host component in uri"
                     | otherwise -> Right (unEscapeString (uriRegName a))

    urlpath = S.pack (uriPath u ++ uriQuery u)

    host = S.pack (uriRegName auth)

    port def = case uriPort auth of
      ""  -> def
      _   -> read $ tail $ uriPort auth :: Word16


--
-- | Creates a basic SSL context. This is the SSL context used if you make an
-- @\"https:\/\/\"@ request using one of the convenience functions. It
-- configures OpenSSL to use the default set of ciphers.
--
-- On Linux, OpenBSD and FreeBSD systems, this function also configures
-- OpenSSL to verify certificates using the system/distribution supplied
-- certificate authorities' certificates
--
-- On other systems, /no certificate validation is performed/ by the
-- generated 'SSLContext' because there is no canonical place to find
-- the set of system certificates. When using this library on such system,
-- you are encouraged to install the system
-- certificates somewhere and create your own 'SSLContext'.
--
{-
    We would like to turn certificate verification on for everyone, but
    this has proved contingent on leveraging platform specific mechanisms
    to reach the certificate store. That logic should probably be in
    hsopenssl, but feel free to change this as appropriate for your OS.
-}
baselineContextSSL :: IO SSLContext
baselineContextSSL = withOpenSSL $ do
    ctx <- SSL.context
    SSL.contextSetDefaultCiphers ctx
#if defined(darwin_HOST_OS)
    SSL.contextSetVerificationMode ctx SSL.VerifyNone
#elif defined(mingw32_HOST_OS)
    SSL.contextSetVerificationMode ctx SSL.VerifyNone
#elif defined(freebsd_HOST_OS)
    SSL.contextSetCAFile ctx "/usr/local/etc/ssl/cert.pem"
    SSL.contextSetVerificationMode ctx $ SSL.VerifyPeer True True Nothing
#elif defined(openbsd_HOST_OS)
    SSL.contextSetCAFile ctx "/etc/ssl/cert.pem"
    SSL.contextSetVerificationMode ctx $ SSL.VerifyPeer True True Nothing
#else
    fedora <- doesDirectoryExist "/etc/pki/tls"
    if fedora
        then do
            SSL.contextSetCAFile ctx "/etc/pki/tls/certs/ca-bundle.crt"
        else do
            SSL.contextSetCADirectory ctx "/etc/ssl/certs"
    SSL.contextSetVerificationMode ctx $ SSL.VerifyPeer True True Nothing
#endif
    return ctx


parseURL :: URL -> URI
parseURL r' =
    case parseURI r of
        Just u  -> u
        Nothing -> error ("Can't parse URI " ++ r)
  where
    r = T.unpack $ T.decodeUtf8 r'

------------------------------------------------------------------------------

{-
    Account for bug where "http://www.example.com" is parsed with no
    path element, resulting in an illegal HTTP request line.
-}

path :: URI -> ByteString
path u = case url of
            ""  -> "/"
            _   -> url
  where
    url = T.encodeUtf8 $! T.pack
                      $! concat [uriPath u, uriQuery u, uriFragment u]


------------------------------------------------------------------------------

--
-- | Issue an HTTP GET request and pass the resultant response to the
-- supplied handler function. This code will silently follow redirects,
-- to a maximum depth of 5 hops.
--
-- The handler function is as for 'receiveResponse', so you can use one
-- of the supplied convenience handlers if you're in a hurry:
--
-- >     x' <- get "http://www.bbc.co.uk/news/" concatHandler
--
-- But as ever the disadvantage of doing this is that you're not doing
-- anything intelligent with the HTTP response status code. If you want
-- an exception raised in the event of a non @2xx@ response, you can use:
--
-- >     x' <- get "http://www.bbc.co.uk/news/" concatHandler'
--
-- but for anything more refined you'll find it easy to simply write
-- your own handler function.
--
-- Throws 'TooManyRedirects' if more than 5 redirects are thrown.
--
get :: URL
    -- ^ Resource to GET from.
    -> (Response -> InputStream ByteString -> IO β)
    -- ^ Handler function to receive the response from the server.
    -> IO β
get r' handler = getN 0 r' handler

getN n r' handler = do
    bracket
        (establish u)
        (teardown)
        (process)

  where
    teardown = closeConnection

    u = parseURL r'

    q = buildRequest1 $ do
            http GET (path u)
            setAccept "*/*"

    process c = do
        sendRequest c q emptyBody

        receiveResponse c (wrapRedirect u n handler)


{-
    This is fairly simple-minded. Improvements could include reusing
    the Connection if the redirect is to the same host, and closing
    the original Connection if it is not. These are both things that
    can be done manually if using the full API, so not worried about
    it for now.
-}

wrapRedirect
    :: URI
    -> Int
    -> (Response -> InputStream ByteString -> IO β)
    -> Response
    -> InputStream ByteString
    -> IO β
wrapRedirect u n handler p i = do
    if (s == 301 || s == 302 || s == 303 || s == 307)
        then case lm of
                Just l  -> getN n' (splitURI u l) handler
                Nothing -> handler p i
        else handler p i
  where
    s  = getStatusCode p
    lm = getHeader p "Location"
    !n' = if n < 5
            then n + 1
            else throw $! TooManyRedirects n


splitURI :: URI -> URL -> URL
splitURI old new' =
  let
    new = S.unpack new'
  in
    if isAbsoluteURI new
       then
            new'
       else
         let
            rel = parseRelativeReference new
         in
            case rel of
                Nothing -> new'
                Just x  -> S.pack $ uriToString id old {
                                                    uriPath = uriPath x,
                                                    uriQuery = uriQuery x,
                                                    uriFragment = uriFragment x
                                                   } ""


data TooManyRedirects = TooManyRedirects Int
        deriving (Typeable, Show, Eq)

instance Exception TooManyRedirects


--
-- | Send content to a server via an HTTP POST request. Use this
-- function if you have an 'OutputStream' with the body content.
--
post :: URL
    -- ^ Resource to POST to.
    -> ContentType
    -- ^ MIME type of the request body being sent.
    -> (OutputStream Builder -> IO α)
    -- ^ Handler function to write content to server.
    -> (Response -> InputStream ByteString -> IO β)
    -- ^ Handler function to receive the response from the server.
    -> IO β
post r' t body handler = do
    bracket
        (establish u)
        (teardown)
        (process)
  where
    teardown = closeConnection

    u = parseURL r'

    q = buildRequest1 $ do
            http POST (path u)
            setAccept "*/*"
            setContentType t

    process c = do
        _ <- sendRequest c q body

        x <- receiveResponse c handler
        return x


--
-- | Send form data to a server via an HTTP POST request. This is the
-- usual use case; most services expect the body to be MIME type
-- @application/x-www-form-urlencoded@ as this is what conventional
-- web browsers send on form submission. If you want to POST to a URL
-- with an arbitrary Content-Type, use 'post'.
--
postForm
    :: URL
    -- ^ Resource to POST to.
    -> [(ByteString, ByteString)]
    -- ^ List of name=value pairs. Will be sent URL-encoded.
    -> (Response -> InputStream ByteString -> IO β)
    -- ^ Handler function to receive the response from the server.
    -> IO β
postForm r' nvs handler = do
    bracket
        (establish u)
        (teardown)
        (process)
  where
    teardown = closeConnection

    u = parseURL r'

    q = buildRequest1 $ do
            http POST (path u)
            setAccept "*/*"
            setContentType "application/x-www-form-urlencoded"

    process c = do
        _ <- sendRequest c q (encodedFormBody nvs)

        x <- receiveResponse c handler
        return x


--
-- | Specify name/value pairs to be sent to the server in the manner
-- used by web browsers when submitting a form via a POST request.
-- Parameters will be URL encoded per RFC 2396 and combined into a
-- single string which will be sent as the body of your request.
--
-- You use this partially applied:
--
-- >     let nvs = [("name","Kermit"),
-- >                ("type","frog")]
-- >                ("role","stagehand")]
-- >
-- >     sendRequest c q (encodedFormBody nvs)
--
-- Note that it's going to be up to you to call 'setContentType' with
-- a value of @\"application/x-www-form-urlencoded\"@ when building the
-- Request object; the 'postForm' convenience (which uses this
-- @encodedFormBody@ function) takes care of this for you, obviously.
--
encodedFormBody :: [(ByteString,ByteString)] -> OutputStream Builder -> IO ()
encodedFormBody nvs o = do
    Streams.write (Just b) o
  where
    b = mconcat $ intersperse (Builder.fromString "&") $ map combine nvs

    combine :: (ByteString,ByteString) -> Builder
    combine (n',v') = mconcat [urlEncodeBuilder n', Builder.fromString "=", urlEncodeBuilder v']


--
-- | Place content on the server at the given URL via an HTTP PUT
-- request, specifying the content type and a function to write the
-- content to the supplied 'OutputStream'. You might see:
--
-- >     put "http://s3.example.com/bucket42/object149" "text/plain"
-- >         (fileBody "hello.txt") (\p i -> do
-- >             putStr $ show p
-- >             Streams.connect i stdout)
--
put :: URL
    -- ^ Resource to PUT to.
    -> ContentType
    -- ^ MIME type of the request body being sent.
    -> (OutputStream Builder -> IO α)
    -- ^ Handler function to write content to server.
    -> (Response -> InputStream ByteString -> IO β)
    -- ^ Handler function to receive the response from the server.
    -> IO β
put r' t body handler = do
    bracket
        (establish u)
        (teardown)
        (process)
  where
    teardown = closeConnection

    u = parseURL r'

    q = buildRequest1 $ do
            http PUT (path u)
            setAccept "*/*"
            setHeader "Content-Type" t

    process c = do
        _ <- sendRequest c q body

        x <- receiveResponse c handler
        return x


--
-- | A special case of 'concatHandler', this function will return the
-- entire response body as a single ByteString, but will throw
-- 'HttpClientError' if the response status code was other than @2xx@.
--
concatHandler' :: Response -> InputStream ByteString -> IO ByteString
concatHandler' p i =
    if s >= 300
        then throw (HttpClientError s m)
        else concatHandler p i
  where
    s = getStatusCode p
    m = getStatusMessage p

data HttpClientError = HttpClientError Int ByteString
        deriving (Typeable)

instance Exception HttpClientError

instance Show HttpClientError where
    show (HttpClientError s msg) = Prelude.show s ++ " " ++ S.unpack msg

{-
    There should probably also be HttpServerError and maybe even
    HttpRedirectError, but as these names don't seem to show up
    in the runtime when raised, not sure it's worth the bother. It's
    not like we'd want anything different in their Show instances.
-}
