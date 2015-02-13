#lang racket/base

(require ffi/unsafe
				 (except-in racket/contract ->)
         "../../ctypes.rkt")
(provide (except-out (all-defined-out)
                     CURLInfo-String
                     CURLInfo-Long
                     CURLInfo-Double
                     CURLInfo-SList
                     CURLInfo-Mask
                     CURLInfo-Typemask
                     def-curl-info
                     def-curl-info*))

(define _CURLInfo _int)

(define-cpointer-type _CURL-pointer)
(define CURLInfo-String #x100000)
(define CURLInfo-Long #x200000)
(define CURLInfo-Double #x300000)
(define CURLInfo-SList #x400000)
(define CURLInfo-Mask #x0fffff)
(define CURLInfo-Typemask #xf00000)

(define-syntax-rule (def-curl-info id base arg)
  (define id (+ base arg)))
(define-syntax-rule (def-curl-info* base ((id arg) ...))
  (begin (def-curl-info id base arg)
         ...))

(def-curl-info*
  CURLInfo-String
  ([Effective-URL 1]
   [Content-Type 18]
   [FTP-Entry-Path 30]
   [Redirect-URL 31]
   [Primary-IP 32]))

(def-curl-info*
  CURLInfo-Long
  ([Response-Code 2]
   [Header-Size 11]
   [Request-Size 12]
   [SSL-VerifyResult 13]
   [Filename 14]
   [Redirect-Count 20]
   [HTTP-ConnectCode 22]
   [HTTPAuth-Avail 23]
   [ProxyAuth-Avail 24]
   [OS-Errno 25]
   [Num-Connects 26]
   [LastSocket 29]
   [Condition-Unmet 35]))

(def-curl-info*
  CURLInfo-Double
  ([Total-Time 3]
   [NameLookup-Time 4]
   [Connect-Time 5]
   [Pretransfer-Time 6]
   [Size-Upload 7]
   [Size-Download 8]
   [Speed-Download 9]
   [Speed-Upload 10]
   [Content-Length-Download 15]
   [Content-Length-Upload 16]
   [Redirect-Time 19]
   [Appconnect-Time 33]))

(def-curl-info*
  CURLInfo-SList
  ([SSL-Engines 27]
   [CookeList 28]
   [CertInfo 34]))

(define _CURLCode
  (_enum
   '(ok = 0
        unsupported-protocol
        failed-init
        url-malformat
        obsolete4
        couldnt-resolve-proxy
        couldnt-resolve-host
        couldnt-connect
        ftp-weird-server-reply
        remote-access-denied
        obsolete10
        ftp-weird-pass-reply
        obsolete12
        ftp-werd-pasv-reply
        ftp-weird-227-format
        cant-get-host
        obsolete16
        ftp-couldnt-set-type
        partial-file
        couldnt-retr-file
        obsolete20
        quote-error
        http-returned-error
        write-error
        obsolete24
        upload-failed
        read-error
        out-of-memory
        operation-timeout
        obsolete29
        ftp-port-failed
        ftp-couldnt-use-rest
        obsolete32
        range-error
        http-post-error
        ssl-connect-error
        bad-download-resume
        file-couldnt-read-file
        ldap-cannot-bind
        ldap-search-failed
        obsolete40
        function-not-found
        aborted-by-callback
        bad-function-argument
        obsolete44
        interface-failed
        obsolete46
        too-many-redirects
        unknown-telnet-option
        telnet-option-syntax
        obsolete50
        peer-failed-verification
        got-nothing
        ssl-engine-notfound
        ssl-engine-setfailed
        send-error
        recv-error
        obsolete57
        ssl-certproblem
        ssl-cipher
        ssl-cacert
        bad-content-encoding
        ldap-invalid-url
        use-ssl-failed
        send-fail-rewind
        ssl-engine-initfailed
        login-denied
        tftp-notfound
        tftp-perm
        remote-disk-full
        tftp-illegal
        tftp-unknownid
        remote-file-exists
        tftp-nosuchuser
        conv-failed
        conv-reqd
        ssl-cacert-badfile
        remote-file-not-found
        ssh
        ssl-shutdown-failed
        again
        ssl-crl-badfile
        ssl-issuer-error
        last)))

(define-cstruct _CURL-SList
  ([data _string]
   [next _CURL-SList-pointer]))

(define-cpointer-type _CURL-HTTPPost-pointer)
(define CURL-HTTPPost*? (flat-named-contract 'CURL-HTTPPost*? CURL-HTTPPost-pointer?))

(define _time_t _int)
(define _CURL-formget-callback (_fun _void _string _size_t -> _size_t))

(define LONG 0)
(define OBJECTPOINT 10000)
(define FUNCTIONPOINT 20000)
(define OFF_T 30000)

(define << arithmetic-shift)

(define CURLPROTO_HTTP (<< 1 0))
(define CURLPROTO_HTTPS (<< 1 1))
(define CURLPROTO_FTP (<< 1 2))
(define CURLPROTO_FTPS (<< 1 3))
(define CURLPROTO_SCP (<< 1 4))
(define CURLPROTO_SFTP (<< 1 5))
(define CURLPROTO_TELNET (<< 1 6))
(define CURLPROTO_LDAP (<< 1 7))
(define CURLPROTO_LDAPS (<< 1 8))
(define CURLPROTO_DICT (<< 1 9))
(define CURLPROTO_FILE (<< 1 10))
(define CURLPROTO_TFTP (<< 1 11))
(define CURLPROTO_ALL (bitwise-not 0))

(def-curl-info*
  LONG
  ([CURLOPT_PORT 3]
   [CURLOPT_TIMEOUT 13]
   [CURLOPT_INFILESIZE 14]
   [CURLOPT_LOW_SPEED_LIMIT 19]
   [CURLOPT_LOW_SPEED_TIME 20]
   [CURLOPT_RESUME_FROM 21]
   [CURLOPT_CRLF 27]
   [CURLOPT_SSLVERSION 32]
   [CURLOPT_TIMECONDITION 33]
   [CURLOPT_TIMEVALUE 34]
   [CURLOPT_VERBOSE 41]
   [CURLOPT_HEADER 42]
   [CURLOPT_NOPROGRESS 43]
   [CURLOPT_NOBODY 44]
   [CURLOPT_FAILONERROR 45]
   [CURLOPT_UPLOAD 46]
   [CURLOPT_POST 47]
   [CURLOPT_DIRLISTONLY 48]
   [CURLOPT_APPEND 50]
   [CURLOPT_NETREC 51]
   [CURLOPT_FOLLOWLOCATION 52]
   [CURLOPT_TRANSFERTEXT 53]
   [CURLOPT_PUT 54]
   [CURLOPT_AUTOREFERER 58]
   [CURLOPT_PROXYPORT 59]
   [CURLOPT_POSTFIELDSIZE 60]
   [CURLOPT_HTTPPROXYTUNNEL 61]
   [CURLOPT_SSL_VERIFYPEER 64]
   [CURLOPT_MAXREDIRS 68]
   [CURLOPT_MAXCONNECTS 71]
   [CURLOPT_CLOSEPOLICY 72]
   [CURLOPT_FRESH_CONNECT 74]
   [CURLOPT_FORBID_REUSE 75]
   [CURLOPT_CONNECTTIMEOUT 78]
   [CURLOPT_HTTPGET 80]
   [CURLOPT_SSL_VERIFYHOST 81]
   [CURLOPT_HTTP_VERSION 84]
   [CURLOPT_FTP_USE_EPSV 85]
   [CURLOPT_SSLENGINE_DEFAULT 90]
   [CURLOPT_DNS_USE_GLOBAL_CACHE 91]
   [CURLOPT_DNS_CACHE_TIMEOUT 92]
   [CURLOPT_COOKIESESSION 96]
   [CURLOPT_BUFFERSIZE 98]
   [CURLOPT_NOSIGNAL 99]
   [CURLOPT_PROXYTYPE 101]
   [CURLOPT_UNRESTRICTED_AUTH 105]
   [CURLOPT_FTPUSE_EPRT 106]
   [CURLOPT_HTTPPATH 107]
   [CURLOPT_FTP_CREATE_MISSING_DIRS 110]
   [CURLOPT_PROXYAUTH 111]
   [CURLOPT_FTP_RESPONSE_TIMEOUT 112]
   [CURLOPT_IPRESOLVE 113]
   [CURLOPT_MAXFILESIZE 114]
   [CURLOPT_USE_SSL 119]
   [CURLOPT_TCP_NODELAY 121]
   [CURLOPT_FTPSSLAUTH 129]
   [CURLOPT_IGNORE_CONTENT_LENGTH 136]
   [CURLOPT_FTP_SKIP_PASV_IP 137]
   [CURLOPT_FTP_FILEMETHOD 138]
   [CURLOPT_LOCALPORT 139]
   [CURLOPT_LOCALPORTRANGE 140]
   [CURLOPT_CONNECT_ONLY 141]
   [CURLOPT_SSL_SESSION_ID_CACHE 150]
   [CURLOPT_SSH_AUTH_TYPES 151]
   [CURLOPT_FTP_SSL_CCC 154]
   [CURLOPT_TIMEOUT_MS 155]
   [CURLOPT_CONNECTTIMEOUT_MS 156]
   [CURLOPT_HTTP_TRANSFER_DECODING 157]
   [CURLOPT_HTTP_CONTENT_DECODING 158]
   [CURLOPT_NEW_FILE_PERMS 159]
   [CURLOPT_NEW_DIRECTORY_PERMS 160]
   [CURLOPT_POSTREDIR 161]
   [CURLOPT_PROXY_TRANSFER_MODE 166]
   [CURLOPT_ADDRESS_SCOPE 171]
   [CURLOPT_CERTINFO 172]
   [CURLOPT_TFTP_BLKSIZE 178]
   [CURLOPT_SOCKETS_GSSAPI_NEC 180]
   [CURLOPT_PROTOCOLS 181]
   [CURLOPT_REDIR_PROTOCOLS 182]))

(def-curl-info*
  OBJECTPOINT
  ([CURLOPT_FILE 1]
   [CURLOPT_URL 2]
   [CURLOPT_PROXY 4]
   [CURLOPT_USERPWD 5]
   [CURLOPT_PROXYUSERPWD 6]
   [CURLOPT_RANGE 7]
   [CURLOPT_INFILE 9]
   [CURLOPT_ERRORBUFFER 10]
   [CURLOPT_POSTFIELDS 15]
   [CURLOPT_REFERER 16]
   [CURLOPT_FTPPORT 17]
   [CURLOPT_USERAGENT 18]
   [CURLOPT_COOKIE 22]
   [CURLOPT_HTTPHEADER 23]
   [CURLOPT_HTTPPOST 24]
   [CURLOPT_SSLCERT 25]
   [CURLOPT_KEYPASSWD 26]
   [CURLOPT_QUOTE 28]
   [CURLOPT_WRITEHEADER 29]
   [CURLOPT_COOKIEFILE 31]
   [CURLOPT_CUSTOMREQUEST 36]
   [CURLOPT_STDERR 37]
   [CURLOPT_POSTQUOTE 39]
   [CURLOPT_WRITEINFO 40]
   [CURLOPT_PROGRESSDATA 57]
   [CURLOPT_INTERFACE 62]
   [CURLOPT_KRBLEVEL 63]
   [CURLOPT_CAINFO 65]
   [CURLOPT_TELNETOPTIONS 70]
   [CURLOPT_RANDOM_FILE 76]
   [CURLOPT_EDGSOCKET 77]
   [CURLOPT_COOKIEJAR 82]
   [CURLOPT_SSL_CIPHER_LIST 83]
   [CURLOPT_SSLCERTTYPE 86]
   [CURLOPT_SSLKEY 87]
   [CURLOPT_SSLKEYTYPE 88]
   [CURLOPT_SSLENGINE 89]
   [CURLOPT_PREQUOTE 93]
   [CURLOPT_DEBUGDATA 95]
   [CURLOPT_CAPATH 97]
   [CURLOPT_SHARE 100]
   [CURLOPT_ENCODING 102]
   [CURLOPT_PRIVATE 103]
   [CURLOPT_HTTP200ALIASES 104]
   [CURLOPT_SSL_CTX_DATA 109]
   [CURLOPT_NETRC_FILE 118]
   [CURLOPT_IOCTLDATA 131]
   [CURLOPT_FTP_ACCOUNT 134]
   [CURLOPT_COOKELIST 135]
   [CURLOPT_FTP_ALTERNATIVE_TO_USER 147]
   [CURLOPT_SOCKETOPTDATA 149]
   [CURLOPT_SSH_PUBLIC_KEYFILE 152]
   [CURLOPT_SSH_PRIVATE_KEYFILE 153]
   [CURLOPT_SSH_HOST_PUBLIC_KEY_MD5 162]
   [CURLOPT_OPENSOCKETDATA 164]
   [CURLOPT_COPYPOSTFIELDS 165]
   [CURLOPT_SEEKDATA 168]
   [CURLOPT_CRLFILE 169]
   [CURLOPT_ISSUERCERT 170]
   [CURLOPT_USERNAME 173]
   [CURLOPT_PASSWORD 174]
   [CURLOPT_NOPROXY 177]
   [CURLOPT_SOCKS5_GSSAPI_SERVICE 179]
   [CURLOPT_SSH_KNOWNHOSTS 183]
   [CURLOPT_SSH_KEYDATA 185]))

(def-curl-info*
  FUNCTIONPOINT
  ([CURLOPT_WRITEFUNCTION 11]
   [CURLOPT_READFUNCTION 12]
   [CURLOPT_PROGRESSFUNCTION 56]
   [CURLOPT_HEADERFUNCTION 79]
   [CURLOPT_DEBUGFUNCTION 94]
   [CURLOPT_SSL_CTX_FUNCTION 108]
   [CURLOPT_IOCTLFUNCTION 130]
   [CURLOPT_CONV_FROM_NETWORK_FUNCTION 142]
   [CURLOPT_CONV_TO_NETWORK_FUNCTION 143]
   [CURLOPT_CONV_FROM_UTF8_FUNCTION 144]
   [CURLOPT_SOCKOPTFUNCTION 148]
   [CURLOPT_OPENSOCKETFUNCTION 163]
   [CURLOPT_SEEKFUNCTION 167]
   [CURLOPT_SSH_KEYFUNCTION 184]))

(define _CURLOpt _int)

(define _CURLformoption
  (_enum
   '(nothing
     copyname
     ptrname
     namelength
     copycontents
     ptrcontents
     contentslength
     filecontent
     array
     obsolete
     file
     buffer
     bufferptr
     bufferlength
     contenttype
     contentheader
     filename
     end
     obsolete2
     stream
     lastentry)))

(define _CURLformcode
  (_enum
   '(ok
     memory
     option-twice
     null
     unknown-option
     incomplete
     illegal-array
     disabled
     last)))
