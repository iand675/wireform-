# Typed header conformance audit

Scope is the 241 current `KnownHeader` instances in Hermes. The audit checks
the field's published grammar, direction and cardinality, acceptance of
representative wire values, full consumption of malformed values, and
rendering through `setHeader`. List codecs also need field-order tests across
repeated lines. Common parsing and rendering paths need timing and allocation
evidence when their implementation changes.

The source and test inventory is a triage aid. A direct import identifies a
place to look for tests, and a self-round-trip alone can miss wire errors.
Run `cabal test hermes:hermes-tests` for the package suite and
`python3 scripts/run-benchmarks.py --only hermes-bench` from the repository
root for the Criterion target.

| Area | Evidence and outstanding work |
| --- | --- |
| HeaderMap and Accept family | Header names compare and hash by their spelling across literal, cached, and parsed construction. Public API regressions cover ordered raw and typed access, field lines, quality values, and quoting. Accept-Language checks RFC 4647 basic ranges and preserves spelling and order. Accept validates whole-type and subtype wildcards. Accept-Post uses the same range grammar and preserves parameters. |
| Representation headers | Content-Type and Accept-Patch use the concrete media-type grammar. The typed codec and client preserve opaque bytes in quoted media parameters and re-quote values when needed. Client callers can parse wildcards explicitly with `parseMediaRange`. Content-Encoding preserves ordered codings across field lines and represents an explicitly empty field. |
| MIME and AtomPub metadata | Content-MD5 validates exact-width canonical Base64. MIME-Version validates its decimal components and ignores comments. Slug validates percent-encoded UTF-8, preserves its wire spelling, and provides decoded text and typed authoring. All three reject repeated field lines. |
| SOAP action and metering | SOAPAction distinguishes an empty field from an empty quoted URI and validates quoted URI references. Meter retains repeated field lines and mixed directive spellings, reads paired arbitrary-precision counts, and checks known directive direction when supplied. |
| Transparent negotiation directives | Negotiate, TCN, and Variant-Vary retain all field lines. Negotiate exposes bounded algorithm versions and RFC keywords. TCN exposes typed response and override directives and preserves extension bytes. Variant-Vary checks that the wildcard stands alone and exposes case-folded field names. |
| Transparent negotiation variants | Accept-Features and Alternates validate their nested grammars, retain field order and exact member spelling, and support typed lookup and authoring. Tests cover opaque extensions, repeated lines, a real Apache response, and violations such as duplicate fallback variants or attributes. |
| Content-Disposition and Content-Language | Content-Disposition rejects duplicate parameter names and validates RFC 8187 syntax, retaining the original extended spelling. The filename accessor prefers decoded UTF-8 or Latin-1 and can fall back to the regular filename. Content-Language shares the RFC 5646 tag checker, with private use and grandfathered forms, an empty-field constructor, and ordered repeated-line parsing. |
| Structured Fields | Shared Boolean, integer, decimal and byte-sequence codecs checked against RFC 9651 and 262 HTTP Working Group scalar vectors. The dictionary codec adds strict strings, dates, display strings, inner lists, and ordered last-value-wins handling for keys and parameters. CDN-Cache-Control, Priority, the RFC 9530 digest family, and Reporting-Endpoints use it. The signature fields enforce unique labels. Cache-Status and Proxy-Status use the shared list grammar and keep extension parameters in wire order. Additional Structured Fields consumers still need review. |
| Certificate fields | Client-Cert is a singleton and Client-Cert-Chain retains the order of its List entries across lines. The combined accessor checks that the chain has a leaf and excludes that leaf. Cert-Not-Before and Cert-Not-After are ACME STAR response fields that use the shared HTTP-date codec and UTCTime. |
| Fetch CORS and Resource Timing | The CORS header and method lists preserve ordered field lines, skip empty members, and parse complete tokens. Response lists represent an explicitly empty field value. The request method, max-age, and credentials fields enforce singleton cardinality. Timing-Allow-Origin combines lists of validated serialized origins, null, and wildcard entries. |
| Early cross-site access and S-HTTP | Method-Check uses the shared case-sensitive method type. Method-Check-Expires resolves all three HTTP-date forms with the configured reference clock. Referer-Root requires an explicit port or the null literal. Security-Scheme validates the S-HTTP/1.4 literal. The four fields enforce singleton lookup. |
| Proxy cache notification | Proxy-Features validates a DNS name, typed capabilities, absolute logging URIs, expiry hours, and six-digit peak masks. Proxy-Instruction validates one of three commands for a DNS name. Both retain the accepted wire spelling and reject repeated field lines. |
| HTTP Extension Framework | Man and Opt decode extension identifiers, namespace prefixes, and token or quoted arguments. They preserve repeated field lines and exact bytes. Ext decodes an empty acknowledgement and rejects a payload or repeated acknowledgement lines. |
| PEP declarations and policies | PEP requires a map and validates extension URIs, prefixes, and strength. PEP-Info requires a policy id and decodes targets, expiry, nested parameters, and strength. Both preserve ordered repeated field lines and unknown bags. |
| PICS and JEPI protocol directives | Protocol and Protocol-Request decode registered PICS forms and August 1996 PEP directives. Protocol-Info and Protocol-Query apply the draft's field-specific strength and delegation rules. All four preserve ordered repeated fields and the published JEPI whitespace-separated form. |
| PICS label lists | PICS-Label distinguishes rating services, service errors, labels, label trees, and rating ranges. The codec validates typed options, inherited generic targets, timestamps, and Base64. It retains the original field bytes and enforces a single field line. |
| P3P and OPS profile fields | P3P validates policy-reference URIs and compact policies, preserves extensions and field order, and exposes the first policy's distinct recognized claims. GetProfile, SetProfile, and ProfileObject validate HTTP field octets, preserve the original bytes, and reject repeated fields. |
| Diagnostic fields | X-Request-ID, X-Correlation-ID, X-Trace-ID, and X-Powered-By validate and preserve opaque field bytes. X-Request-Start preserves its original spelling and exposes an exact decimal without guessing the sender's time unit. All five enforce singleton cardinality. |
| AMP cache transforms | AMP-Cache-Transform validates its draft Structured Headers grammar and preserves every physical field line. The codec exposes ordered alternatives, disjoint version intervals, and typed response selection. |
| Browser isolation policies | The opener and embedder policies and both report-only fields validate Structured Fields Items and project `report-to` when its final value is a string. Resource policy validates the exact Fetch tokens. Origin-Agent-Cluster decodes a Boolean Item. All six apply HTTP field whitespace and enforce a single field line. |
| Content Security Policy | The enforce and report-only fields keep separate policies across commas and physical field lines. Each policy retains directive order and has a first-occurrence projection with ASCII case folding. |
| EventSource and declarative refresh | Last-Event-ID validates UTF-8, preserves the opaque identifier, and rejects repeated lines. Refresh extracts a bounded whole-second delay and an optional target using the HTML refresh syntax. It preserves target octets and selects the first field line. |
| Connection negotiation | ALPN combines all list field lines, enforces canonical percent escapes, and bounds decoded identifiers. HTTP2-Settings enforces one field line and complete settings entries. Its typed view preserves unknown setting IDs and repeated updates in wire order. |
| Browser framing, sniffing, and site-data fields | X-Frame-Options validates singleton directives and exposes the origin of a legacy ALLOW-FROM address. X-Content-Type-Options has a singleton token codec and a Fetch-compatible first-member query. Clear-Site-Data reads the ordered list across field lines, recognizes `clientHints`, and preserves quoted extension bytes. |
| Legacy browser controls | X-DNS-Prefetch-Control folds ASCII case. X-Download-Options accepts the documented `noopen` value. X-Permitted-Cross-Domain-Policies supports a domain policy and an optional response-local directive across field lines. X-XSS-Protection validates reporting URI-references and rejects duplicate directives. |
| Edge surrogate fields | Surrogate-Capability combines ordered device entries, checks uniqueness, and exposes typed capability products. Surrogate-Control retains ordered device targets, validates the freshness and content arguments, and preserves opaque extension value bytes. |
| Client hints and request signals | Accept-CH validates field-name tokens and keeps explicit empty opt-ins. Sec-Purpose retains the Structured Fields List and projects prefetch parameters. Capsule-Protocol recognizes Boolean Items and ignores unknown parameters after validation. Early-Data decodes any present line as the RFC 8470 marker. |
| Web Push | TTL works in both directions and applies the specified overflow value. Topic enforces the 32-character cap and preserves spelling. Urgency recognizes the four case-insensitive tokens. All three enforce singleton field lines. |
| Browser reporting | NEL selects the first valid JSON policy in field order and exposes typed registration and removal. Reporting-Endpoints applies dictionary replacement before checking URI references. |
| Privacy and data preferences | Sec-GPC recognizes an exact assertion on any physical field line. DNT validates its singleton preference digit and optional extension. Save-Data retains the ordered Token Items and projects the last preference. |
| Permissions policy | The full Structured Fields dictionary is retained and duplicate feature names use the last value across field lines. The typed declaration view validates CSP scheme and host sources, reads Token-valued reporting endpoints, and distinguishes an empty allowlist from an unsupported member value. |
| Conditional entity tags | ETag, If-Match, If-None-Match, If-Range, and Delta-Base preserve every allowed opaque octet. ETag and Delta-Base enforce singleton fields. Match lists preserve field order, and the HTTP client uses the same strong and weak byte comparisons as Hermes. |
| Delta encoding | A-IM and IM combine field lines in order, preserve extension parameters, and retain empty lists. A-IM uses the shared quality-value renderer and accepts the reserved identity token. IM rejects identity. |
| Range requests and partial responses | Range recognizes the byte unit case-insensitively, skips empty list entries, and validates offsets through the full Word64 range. Content-Range enforces ordered endpoints and a complete length beyond the last byte. Both enforce singleton field lines. Accept-Ranges keeps advertised units in order, and the client projects only byte units into its byte-range type. |
| WebSocket handshake fields | Key and Accept decode canonical Base64 at their prescribed lengths. Version validates the 0–255 grammar, and Protocol checks the uniqueness and order of offered tokens. Explicit request and response settings select the singleton grammar for request Version and response Protocol. Extensions preserves order across lines in both directions and validates parameter tokens after unescaping. |
| W3C Trace Context | Traceparent checks version, lowercase hex and nonzero identifiers, rejects repeated field lines and preserves future-version extensions. Tracestate combines field lines in order, recognizes simple and tenant keys, preserves spaces inside values, rejects duplicate keys and enforces the combined member limit. |
| HTTP dates and delays | Typed lookup covers all three HTTP-date formats, calendar and clock bounds, leap seconds, the explicit RFC 850 clock cutoff, field whitespace and singleton cardinality. The clock-free API retains its 1970–2069 window. Age uses the first field member and saturates overflow, Cache-Control accepts quoted delays and saturates overflow, and Retry-After retains decimal delays beyond Word. Cookie Expires has its own token grammar, year floor and clock rules. |
| Cache-Control | The codec handles all RFC 9111 directives, ASCII case-insensitive names, ordered repeated field lines, empty lists, quoted field-name lists, and opaque extension bytes. Qualified directives have typed field-name builders and a typed accessor. CDN-Cache-Control applies the separate RFC 9213 dictionary grammar and checks known directive value types. |
| HTTP field lists | Allow, Vary, Connection, Keep-Alive, Trailer, Upgrade, and Accept-Ranges combine repeated field lines in wire order and skip empty members. Tests cover method and wildcard token boundaries, case-folded Vary names, quoted Keep-Alive parameters, empty field values, and the reserved Accept-Ranges value `none`. Vary, Connection, Trailer, and Upgrade keep their existing populated constructors and add explicit empty constructors. |
| Product and comment fields | User-Agent and Server share the required product spacing, comment grammar, and singleton field handling. Via shares the comment codec and combines repeated field lines in order. Nested and escaped comments canonicalize to escaped opaque text, with a one-to-one Latin-1 mapping for wire bytes. Via retains its populated constructor and adds `ViaEmpty`. |
| Content-Length | Typed lookup consumes the whole singleton field and detects overflow before accumulating the next digit. Tests cover leading zeroes, the largest representable value, oversized input, and rejection of repeated lines and comma lists. |
| Authorities and origins | Host, Origin, Access-Control-Allow-Origin, Via, Alt-Used, and Alt-Svc share bracketed IP-literal validation, registered names and bounded decimal ports. Via also accepts a full HTTP token as a pseudonym. Host supports the empty field and normalizes an empty URI port to omission. Origin has a typed legacy RFC 6454 list, and Access-Control-Allow-Origin follows Fetch's single-origin, null or wildcard grammar. The typed codec preserves host spelling. HTTP/1 validates Host and CONNECT authorities using the shared grammar. See the [transport checks](#http1-host-and-connect-authorities). |
| Forwarding fields | Forwarded validates registered parameters and per-hop name uniqueness while preserving extension bytes and ordered field lines. X-Forwarded-For combines the full chain. X-Forwarded-Host validates authorities, X-Forwarded-Port bounds transport ports, and X-Forwarded-Proto checks URI schemes. X-Real-IP requires a single IPv4 or IPv6 address. Max-Forwards enforces singleton cardinality and the representable hop-count range. |
| WebDAV core and search | If decodes tagged and untagged alternatives into a condition tree and preserves opaque ETag octets. Destination and Lock-Token validate URI syntax. DAV, DASL, and Timeout combine ordered field lines. Timeout checks the 32-bit limit, and Depth and Overwrite enforce singleton cardinality. |
| WebDAV extensions | Label validates URL-escaped UTF-8 and provides a decoded text view. Position validates a single path segment. Ordering-Type requires an absolute URI, while Redirect-Ref validates a URI-reference. Apply-To-Redirect-Ref checks its flag, and Status-URI checks three-digit status codes and coded URLs. All six enforce singleton cardinality. |
| CalDAV scheduling and attachments | Schedule-Tag and If-Schedule-Tag-Match preserve quoted wire bytes. Schedule-Reply and CalDAV-Timezones share the WebDAV flag codec. Cal-Managed-ID validates iCalendar paramtext and is response-only. All five enforce singleton cardinality. |
| Repeatable requests | Both identifier fields preserve UUID spelling and support opaque HTTP tokens. Repeatability-Result accepts both values with ASCII case folding. Repeatability-First-Sent reads HTTP dates and emits IMF-fixdate. All four apply field OWS and reject repeated lines. |
| OData version and isolation | OData-Version follows the OASIS wire grammar, including the zero in `4.01`. OData-MaxVersion preserves fractional precision and rejects numeric overflow. Isolation and OData-Isolation share case-insensitive snapshot parsing and preserve unsupported tokens. All four enforce singleton cardinality. |
| OData entity references | OData-EntityId validates absolute and relative RFC 3987 references, checks raw UTF-8, preserves accepted spelling, and maps Unicode to URI escapes on output. Field lookup rejects empty and repeated lines. |
| OSLC configuration and version | Configuration-Context is request-only and validates the RFC 3986 URI grammar, including fragments. OSLC-Core-Version validates its two decimal components, enforces a major version of at least 2, and offers arbitrary-precision numeric access. Both preserve spelling and enforce singleton cardinality. |
| Content identifiers | Content-ID accepts classic MIME message IDs, OData batch request IDs, and composite DRP identifiers. Differential-ID shares the DRP grammar. Both retain accepted spelling and enforce singleton cardinality. |
| Legacy URI fields | Content-Base validates a single absolute URI. The URI list validates bracketed references, skips empty list slots, and combines repeated lines in order. Both preserve accepted spelling. |
| Legacy version tags | Content-Version and Derived-From use HTTP quoted strings. They preserve decoded octets, canonicalize quoted pairs when rendering, and enforce singleton cardinality. |
| Legacy HTML metadata | Content-Script-Type and Content-Style-Type select the final field line, validate its media type and parameters, and retain its wire spelling. Default-Style selects the final title and emits a quoted string. |
| Transfer codings | Transfer-Encoding and TE combine repeated lines in order, preserve extension parameters, and represent empty fields explicitly. Transfer-Encoding rejects repeated chunked codings and parameters on standard codings. Its request framing check requires a final chunked coding. General parsing also supports close-delimited responses. TE treats trailers as a bare keyword, rejects chunked, and renders explicit weights in the shared qvalue grammar. Quoted parameter bytes are covered across all 256 octets. HTTP/1 uses the typed coding list to determine framing. See the [transport checks](#http1-transfer-framing). |
| Authentication | Authorization and Proxy-Authorization accept bare schemes, strictly lex token68, and enforce singleton fields. Challenge fields combine physical lines before disambiguating commas and reject duplicate parameter names within a challenge. Authentication-Info and Proxy-Authentication-Info use the same token and quoted-string parameter codec. Opaque bytes round-trip and project into the HTTP client. Authentication-Control combines field lines, validates RFC 8053 names and extended values, and rejects repeated parameter names within an entry. |
| Proof, nonce, and registration fields | DPoP, DPoP-Nonce, and Replay-Nonce enforce their singleton wire grammars and keep the accepted bytes. Hobareg uses the RFC 7486 status words and treats malformed or repeated values as in progress. Optional-WWW-Authenticate combines challenges across lines and omits an empty authored list. |
| OSCORE and Token Binding | OSCORE maps unpadded base64url to CoAP option bytes, including the empty option. Sec-Token-Binding exposes ordered binary entries and validates message framing, known key and signature sizes, and required binding types. Include-Referred-Token-Binding-ID is a case-insensitive response signal. All three reject repeated field lines. |
| URI-valued fields | Location, Content-Location, and Referer share RFC 3986 reference validation and preserve the accepted spelling. Content-Location and Referer exclude fragments, and Referer excludes userinfo. Ping-From and Ping-To use the browser serializer's ASCII component syntax, including opaque paths and its distinct query rules. The browser URL codec validates wire syntax and leaves URL resolution and host normalization to callers. All five reject repeated lines. |
| Request Cookie | Tests cover all 256 octets in bare and quoted values, Base64 punctuation, empty values, preservation of quotes, and ordered recombination of HTTP/2 and HTTP/3 field lines. A singleton empty field remains readable as an empty list. |
| Response Set-Cookie | The codec shares request Cookie's exact value grammar. Standard attribute names and SameSite values are case-insensitive. Tests cover signed and saturated Max-Age, raw paths, duplicate attributes, ordered extension preservation, and separate physical lines. A focused client middleware test covers storing and returning quoted values, host and path matching, rejecting an out-of-scope domain, and expiration. Expires supports RFC 6265 date tokens with the first syntactic match for each component and stops at the next attribute. |
| Legacy cookie fields | Cookie2 checks singleton decimal versions. Set-Cookie2 parses folded lists within each line, retains field order and bytes, and projects the first occurrence of each attribute. Typed authoring validates required versions, quoted URL and port arguments, and opaque values. |

The source inventory now finds 241 instances with a direct import in Hermes
tests. Direct imports do not establish the depth of the wire cases or
interoperability. Downstream and aggregate coverage still need inspection.

The latest GHC 9.10.3 suite passed all 1,122 Hermes tests. The earlier
748-test suite passed 100 of 100 threaded runs with two or four
capabilities after switching map keys to byte equality and hashing.
Before the switch, the suite intermittently
returned `Right Nothing` for a present Content-Type or
Access-Control-Allow-Origin. Six representative field names kept their
previous Binary encoding and Symbol conversion. The earlier Structured
Fields check passed all 262 upstream scalar vectors. The shared token scanner
cut allocation in the three-entry Accept benchmark from 2,976 to 1,664 bytes.

The date renderer matched `Data.Time` for 3,068,039 Gregorian days from the
end of 1599 through the start of 10000, both as a typed field and as part of
a larger builder. The canonical typed Date Criterion fixture went from
203 to 123 ns for a fully forced parse and from 3,402 to 26 ns for rendering.
Allocated bytes per operation went from 1,664 to 1,136 and from 24,128 to
160, respectively. Go's standard library took 79–99 ns to parse the same
fixture and 74–78 ns to render it on the same host. The Hermes parse checks
weekday consistency and runs through the generic typed field driver.
Age and Retry-After took 37 and 38 ns respectively for four-digit delays,
with 552 bytes allocated per operation in each case.

The typed field-list Criterion fixtures went from 73 to 58 ns for Allow
and from 124 to 96 ns for Connection. Canonical single-line Vary went from
286 to 53 ns, and repeated Vary went from 504 to 202 ns. Allocation per
operation fell from 840 to 544 bytes for Allow, 856 to 656 for Connection,
1,489 to 520 for single-line Vary, and 3,457 to 2,025 for repeated Vary.
The measurements cover these fixtures, including canonical lowercase
common field names in Vary.

Comment validation agreed with an independent recursive implementation on
2,396,745 inputs. The suite exercises all 256 octets both literally and
after a quoting backslash, and checks 32,768 levels of nested parentheses.
The Criterion parse fixtures went from 89 to 75 ns for bare User-Agent,
199 to 131 ns for Server with a comment, 182 to 118 ns for Via with a
comment, and 47 to 38 ns for a five-digit Content-Length. These parse
fixtures all allocated less, while the Server render fixture went from
95 to 88 ns and allocated 32 more bytes.

The regular hexadecimal IPv6 parser now packs hextets in two words.
It matched Go's `netip` on acceptance and decoded address bits for 72,886
inputs. Typed bracketed Host accepted the same set. On the measured
bracketed fixture, typed Host went from 235 to 126 ns during this pass,
and from 2,824 to 1,336 allocated bytes. Go's fixed-size address-and-port
parser took 26–27 ns. Hermes also validates the field and retains its
host spelling. [IPv6 Host scanning](#ipv6-host-scanning) updates this
comparison. Regular
Host went from 101 to 96 ns, regular Origin from 132 to 117 ns, and Via
with a port from 110 to 142 ns after adding authority validation.

The tuned Transfer-Encoding fixture went from 98 to 85 ns, allocating
912 bytes per operation versus 848 before. The TE fixture went from
129 to 76 ns and from 1,000 to 632 bytes. The extension fixture parsed
in about 281 ns. Tests round-trip all 1,001 qvalues and the shared
comment scanner continued to agree with the independent implementation
on all 2,396,745 fixtures.

The token68 regression checks all 256 octets before and after padding.
The Criterion token68 credential fixture went from 133 to 89 ns and from
896 to 632 allocated bytes. The mixed challenge fixture went from 460 to
315 ns and from 2,928 to 1,832 bytes. The extracted client projection
compiled against Hermes and passed repeated-line, opaque-byte, and malformed
credential cases. The offline environment could not solve the full
`wireform-http` build because it lacks `crypton >= 2.2.6 && < 2.3`.

The RFC 3986 codec agreed with an independently written grammar on 38,603
Location and Referer fixtures, including rejection and byte-preserving
rendering. The Ping-To codec preserved 2,029 distinct URLs serialized by
Node's WHATWG URL implementation. Criterion measured the new typed parses
at about 68 ns for relative Location, 107 ns for absolute Referer, and
155 ns for Ping-To with its String value forced. The previous opaque
parsers took about 49, 48, and 123 ns. Per-operation allocation went from
696 to 576 bytes, 712 to 592 bytes, and 2,152 to 2,088 bytes. The inner URI
character scanners use unboxed offsets and bit tests in simplified GHC Core.
Go's `net/url.Parse` took 104–106 ns, 115–125 ns, and 101–110 ns on the
same inputs, returning parsed URL components and allocating 160, 144, and
144 bytes. The comparison leaves an allocation gap, particularly for the
Ping fields' existing `String` representation.

The typed request Cookie fixture went from 266 to 162 ns to parse and
from 191 to 106 ns to render. Allocations fell from 1,304 to 960 bytes
and from 1,831 to 855 bytes, respectively. The Base64 and split-line
fixtures parsed in 151 and 187 ns, allocating 952 and 1,088 bytes.
Go's request Cookie parser took 160–169, 159–163 and 173–180 ns on
the three parse fixtures, allocating 600 bytes per operation. Joining
repeated lines once with the RFC separator removed roughly 920 bytes
per parse compared with parsing each line and appending the lists.
The octet-scanning loop uses unboxed offsets in GHC Core.

The HeaderMap construction fixture went from 429 to 73 ns for four
fields and from 552 to 187 ns for seven fields with repeated names.
Allocations went from 2,096 to 832 bytes and from 3,896 to 1,792 bytes.
The small typed lookup fixture took 81 ns after the switch and 79 ns
before it, at 744 allocated bytes in both cases.

The response Set-Cookie fixture with a single pair went from 117 to 70 ns
and from 976 to 680 allocated bytes. The attribute fixture went from
249 to 278 ns while allocation fell from 1,992 to 1,536 bytes. The new
quoted cookie with a path, age and policy took 278 ns and 1,656 bytes.
Go's `net/http.ParseSetCookie` took 54–58, 269–336 and 170–188 ns on
these fixtures, allocating 208, 312 and 272 bytes. Hermes copies strings
into compact owned values, while Go keeps slices of the original string.
Both the allocation gap and the attribute scan remain worth measuring.

All 14 client cookie tests passed in a focused build of the repository's
client middleware and its dependencies. The full offline `wireform-http`
test plan still requires `crypton >= 2.2.6 && < 2.3`.

The cookie expiry parser agreed with an independent RFC 6265 date-token
implementation on 80,616 inputs. The canonical Expires fixture took
205 ns versus 218 ns before, with roughly 1.5 KB allocated in either
case. The token-format fixture took 256 ns and allocated 1,592 bytes
after making the cursor and parser state explicit in the recursive
worker. Ordinary HTTP Date parsed in 128 ns versus 129 ns.

Accept-Language agreed with an independent RFC 4647 basic-range grammar
on 24,390 inputs. Its four-range fixture went from 186 to 141 ns with
1,072 bytes allocated in both cases. Simplified GHC Core keeps the
subtag width and input cursor unboxed through the scanner.

Concrete media types and wildcard ranges agreed with an independent
grammar on 54,338 inputs. Accept-Patch retains each entry's parameters
in `acceptPatchMediaRanges`. The earlier constructor, record updates
and bare-type selector still compile. Accept-Post has an empty-field
constructor and a total `acceptPostEntryList` accessor. It accepts the
legacy bare wildcard alongside the standard media-range spellings.
The focused client test build passed 20 cookie and media tests.

The quoted multipart Content-Type parse went from 340 to 190 ns and
from 3,016 to 1,248 allocated bytes. Go's `mime.ParseMediaType` took
137–182 ns and allocated 344 bytes on the same input. The Accept parse
fixture took 250–263 ns versus 258 ns before, with 24 additional bytes
allocated per operation. Rendering the quoted multipart Content-Type
went from 260 to 109 ns and from roughly 2,336 to 1,000 allocated bytes.
Accept rendering stayed near 250 ns and allocated about 2,287 bytes.

The extended-value codec agreed with an independent grammar and decoder
on 24,960 cases, including every escaped octet. The language checker
agreed on 36,365 cases. Quoted ASCII Content-Disposition parsing went
from 427 to 145 ns and from 3,752 to 800 allocated bytes. The extended
field parsed in 334 ns and allocated 1,392 bytes. Decoding the preferred
filename from that field took another 277 ns. Go's `mime.ParseMediaType`
took 172–214 ns for the plain field and 448–459 ns for the extended
field with decoding. The Hermes typed result preserves wire spelling
and postpones decoding until requested.

The Content-Language fixture took 171 ns versus 169 ns before validation,
with 936 allocated bytes versus 1,096. The common language, script and
region path uses an indexed scan with an unboxed offset in simplified
GHC Core.

Authentication-Control matched an independent parameter-name grammar
and generated field-boundary expectations on 19,771 cases. It now
validates extended values and preserves their wire spelling. It rejects
duplicates within an entry, including plain and extended spellings of
the same name. Selecting a relevant scheme and realm is a caller
decision. The two-entry fixture took 291 ns versus 865 ns before, with
about 1,568 versus 5,528 allocated bytes. The extended-value fixture
took 288 ns versus 446 ns and allocated about 1,296 versus 3,280 bytes.
The repeated-field fixture took 343 ns and allocated about 1,760 bytes.

Content-Encoding uses `ContentEncodingEmpty` for a present field with
no codings. The `contentCodings` accessor returns an ordered list
that is empty in that case. The empty field took about 37 ns and
allocated 472 bytes. The populated chain took 60–80 ns versus 59–79 ns
before and allocated 544 versus 648 bytes.

Entity tags use compact opaque bytes. The existing `StrongETag` and
`WeakETag` text patterns construct or match UTF-8 tags. The byte
constructors cover all valid wire tags. Both parser entry points agreed
with the grammar on 131,072 strong and weak two-byte inputs. The 36
focused client tests passed, including exact rendering and comparison
of distinct non-UTF-8 validators. The full client suite requires a
`crypton` version unavailable to the offline solver.

The strong and weak standalone fixtures took 30 and 32 ns versus 179
and 176 ns before. Allocation fell from about 2,072 to 200 bytes per
operation on both fixtures. The opaque fixture took 34 ns with 208
allocated bytes. Go's `net/http` scanner with a copy of the tag body
took 21–23 ns for ASCII and 25–31 ns for opaque bytes. The shared Hermes
scan uses unboxed addresses and allocates nothing per byte in simplified
GHC Core. The typed result still allocates more than Go's string result.

Cache-Control matched 5,624 generated cases covering name casing and
boundaries, sparse lists, delay arguments, and all quoted octets with
and without a quoted-pair escape. The HTTP client cache test covers a
mixed-case no-store directive on a separate field line. All 43 focused
client tests and 70 focused Core parser tests passed. The broader
Core suite reported 72 transport ring creation failures.

The canonical three-directive parse took 127 ns versus 95 ns before,
and allocated 1,024 versus 920 bytes. It now checks the whole directive
name, folds ASCII case in the dispatch, and accepts sparse RFC lists.
Generating both literal cases to share one branch brought this fixture
down from 214 ns for the first implementation. The qualified fixture
went from 386 to 228 ns and from 4,351 to 1,752 allocated bytes. Its
quoted output rendered in 104 ns. Go's pquerna/cachecontrol v0.2.0 took
89–96 ns and allocated 80 bytes on the canonical input. Its qualified
parse took 190–192 ns and allocated 440 bytes. Go returns a directive
record, while Hermes retains list order and duplicate directives.

The shared dictionary codec matched all 433 HTTP Working Group
dictionary cases, including ordered duplicates and the large generated
dictionaries. A further 312 date, display-string, and string vectors
matched when embedded as dictionary members. The common path avoids
an ordered map allocation for up to three distinct keys. The string
scanner uses an unboxed cursor and escape flag in simplified GHC Core.

CDN-Cache-Control validates the final value of a repeated key, retains
extension members and parameters, and projects understood directives
for caching policy. The existing directive constructor still builds
ordinary policies. Use `cdnCacheControlDirectiveList` to read parsed
values and `renderCDNCacheControlChecked` to check constructed policies.
The 43 focused HTTP client tests also passed.

The canonical typed parse took 216–220 ns versus 125 ns for the previous
Cache-Control grammar and allocated 1,672 versus 1,096 bytes. The
parameterized and repeated-line fixtures took 317–320 ns and 368–375 ns.
Go's httpsfv v1.1.2 with the same directive projection took 380–382 ns,
472–503 ns, and 445–457 ns on those fixtures. Hermes allocated more
than Go on all three. The checked renderer from the directive
constructor took 188 ns, while Go rendered a prepared dictionary
in 113–115 ns.

Priority now combines every field line and applies the final dictionary
member for each key. Unknown members and syntactically valid values of
an unexpected type or range leave the applicable priority default. The
digest fields require byte sequences for integrity values and integers
from 0 through 10 for preferences. They preserve parameters, resolve
duplicate keys before checking field-specific types, and omit empty
dictionaries when generating headers. The existing populated constructors
are available alongside total entry-list accessors for received fields.

The independent Priority and digest projection matrix matched 12,434
cases. The refactored dictionary normalization still matched all 433
upstream dictionary cases and 312 embedded date and string cases.
Simplified GHC Core carries Priority's urgency as an unboxed integer
through projection. Digest normalization uses the ShortText-specialized
worker for both algorithm keys and parameters.

Across two sequential Criterion runs, canonical Priority parsing took
623–757 ns versus 726–789 ns before the change and allocated 928 versus
1,000 bytes. Canonical Content-Digest parsing took about 1.43 µs versus
1.46–1.48 µs and allocated 1,400 versus 1,408 bytes. Go's httpsfv v1.1.2
with the same typed projection took 1.35–1.54 µs for Priority and
1.28–1.40 µs for Content-Digest, allocating 624 and 560 bytes respectively.
Content-Digest rendering took 0.80–0.99 µs and 1,296 bytes in Hermes,
compared with 0.56–0.70 µs and 168 bytes in Go.

The three RFC 9421 signature fields combine repeated lines and enforce
unique labels. Signature-Input and Accept-Signature preserve component
and parameter order, reject equivalent duplicate components even when
their parameters are reordered, and distinguish timestamp integers
from negotiation flags. Their inner lists accept ASCII spaces.
Reporting-Endpoints applies dictionary key replacement before discarding
non-String values and malformed URI references. It ignores parameters
and preserves the spelling of relative and absolute references.
The independent matrix passed all 80,292 cases for these four fields.
After the final renderer change, all 433 upstream dictionary vectors,
312 embedded scalar vectors, and 43 focused client tests also passed.

The shared String renderer copies ordinary ASCII spans in bulk and
escapes quotes and backslashes at their original positions. Simplified
GHC Core shows direct byte scanning for ordinary spans and an
allocation-free name comparison loop for short signature component
lists.

Sequential runs of the `signature-reporting` Criterion fixtures on
GHC 9.10.3 and the matching Go httpsfv v1.1.2 fixtures measured the
following. Timing varied between runs. Allocation figures are rounded
bytes per operation.

| Fixture | Previous Hermes time / bytes | Current Hermes time / bytes | Go time / bytes |
| --- | ---: | ---: | ---: |
| Signature parse | 1.34 µs / 1,680 | 0.39 µs / 1,408 | 0.41–0.43 µs / 592 |
| Signature-Input parse | 3.78 µs / 18,392 | 0.78 µs / 2,344 | 1.67–1.69 µs / 1,960 |
| Signature-Input render | 4.28 µs / 17,648 | 0.81 µs / 4,352 | 0.73–0.74 µs / 552 |
| Accept-Signature parse | 3.34 µs / 18,120 | 0.99 µs / 2,304 | 1.61 µs / 1,904 |
| Reporting-Endpoints parse | 1.66 µs / 10,856 | 0.93 µs / 2,864 | 1.12–1.30 µs / 1,056 |

Cache-Status and Proxy-Status combine physical lines as one Structured
Fields List. Parameter replacement takes effect before projection.
Cache-Status preserves its accumulated errors for known parameters,
supports signed TTLs, and retains unknown parameters in wire order.
Proxy-Status keeps the original parameter representation and offers a
typed view of the five parameters in RFC 9209 section 2.1. Both omit
empty lists when generating a field.

Client-Cert rejects repeated field lines. The chain codec parses each
complete line without copying the combined Base64 input. The joint
lookup checks the relationship between the decoded fields. The ACME
STAR validity fields canonically render IMF-fixdate and store UTCTime,
including the leap-second representation of the shared HTTP-date codec.

The Base64 parser now finds the closing delimiter with `memchr` and
passes an input slice to the shared Wireform decoder. Simplified GHC
Core confirms this path. It still accepts omitted padding and rejects
invalid padding and invalid alphabet bytes. The independent matrix
passed 62,193 cases, including two DER fixtures decoded independently,
and the 262 scalar, 433 dictionary, 312 embedded scalar and 43 focused
client checks all passed.

The `status-and-certificates` Criterion group measured the following
against the pre-change Hermes codecs and matching Go fixtures on the
same host. The certificate fixture decodes to 750 bytes per
certificate. Allocation figures are rounded bytes per operation.

| Fixture | Previous Hermes time / bytes | Current Hermes time / bytes | Go time / bytes |
| --- | ---: | ---: | ---: |
| Cache-Status parse | 908 ns / 6,184 | 750 ns / 4,520 | 573–574 ns / 1,184 |
| Proxy-Status parse | 894 ns / 7,727 | 394 ns / 2,648 | 577–586 ns / 1,264 |
| Proxy-Status typed projection and parse | — | 622 ns / 3,552 | 612 ns / 1,264 |
| Client-Cert parse | 2.75 µs / 5,782 | 217 ns / 1,446 | 713–728 ns / 872 |
| Client-Cert-Chain parse | 5.80 µs / 11,973 | 473 ns / 3,173 | 1.64–1.67 µs / 3,888 |
| Cert-Not-After HTTP-date parse | — | 115 ns / 1,136 | 79–80 ns / 0 |

The status and byte-sequence Go fixtures use httpsfv v1.1.2. The
ACME STAR fixture uses Go's `http.ParseTime`.

The Fetch CORS response list codecs distinguish an explicitly empty
value from a missing field. The request header list requires a member.
The compatibility pattern `RequestHeadersWildcard` denotes the
literal field name `*`. Allow-Credentials accepts exactly `true`
and emits zero field lines for a false value. Access-Control-Max-Age
uses non-negative delta-seconds, saturates overflow at the largest
representable Int, and renders a negative constructor input as zero.
Timing-Allow-Origin validates the authority without consuming the comma
that starts the next member. It preserves the received origin spelling.

The 50,075-case CORS and Resource Timing matrix exercises public
typed lookup and canonical rendering. It includes every byte value in
the HTTP token grammar and combines three physical field lines.
Another 55,440 cases verify exact and saturating decimal parsing with
arbitrary bounds and malformed suffixes. Simplified GHC Core confirms
that the decimal loop keeps its accumulator unboxed. The 43 focused
HTTP client tests also passed.

Sequential measurements of the `cors` Criterion fixtures compared
the previous and current Hermes codecs with Go fixtures using byte
token validation, `strconv.ParseUint`, and `net/url.Parse`.
Allocation figures are rounded bytes per operation.

| Fixture | Previous Hermes time / bytes | Current Hermes time / bytes | Go time / bytes |
| --- | ---: | ---: | ---: |
| Allow-Headers parse | 139 ns / 880 | 134 ns / 704 | 69–70 ns / 80 |
| Allow-Headers across two lines | — | 161 ns / 1,232 | 67–70 ns / 80 |
| Request-Method parse | 64 ns / 736 | 47 ns / 536 | 10 ns / 5 |
| Max-Age parse | 44 ns / 688 | 29 ns / 472 | 8.7–8.9 ns / 0 |
| Timing-Allow-Origin parse | 191 ns / 992 | 155 ns / 712 | 252–257 ns / 384 |

### Whole-input parser and bounded reads

The shared whole-input runner calls the parser directly, avoiding the
streaming prompt and intermediate step value. Fixed-input reads honor the
end of a subspan even when the backing buffer has more bytes. Streaming
reads retain their refill behavior. Regression tests cover sliced inputs,
an exact span read, short reads through three primitives, and a streaming
checkpoint used with whole input. The Timing-Allow-Origin field test
covers percent-encoded commas and malformed escapes. The Structured
Fields suite checks all 256 byte values in both the initial and
subsequent positions of dictionary keys and tokens.

Simplified GHC Core shows a direct call from the whole-input runner and
a shared host character table in Timing-Allow-Origin. The full Hermes
and Wireform core suites passed 812 and 165 tests, respectively. The 43
focused client tests, CORS and decimal matrices, and 745 structured-field
dictionary checks also passed.

The same `cors` Criterion fixtures measured the following with GHC 9.10.3.
Allocations are rounded bytes per parse.

| Fixture | Before runner change | With runner and shared host table | Go |
| --- | ---: | ---: | ---: |
| Allow-Headers | 133 ns / 704 B | 113 ns / 504 B | 70 ns / 80 B |
| Allow-Headers across two lines | 167 ns / 1,232 B | 134 ns / 832 B | 70 ns / 80 B |
| Request-Method | 46 ns / 536 B | 34 ns / 336 B | 9.5–9.7 ns / 5 B |
| Max-Age | 30 ns / 472 B | 20 ns / 272 B | 8.6 ns / 0 B |
| Timing-Allow-Origin | 149 ns / 712 B | 121 ns / 512 B | 256–261 ns / 384 B |

At that checkpoint Request-Method and Max-Age took more than twice the
Go time. The core benchmark for 100,000 length-prefixed messages went from 81 µs
to 70 µs. FlatParse took 47 µs on the same fixture.

The 94-fixture Hermes benchmark sweep also exercised structured
dictionaries and authentication. Structured Field key parsing uses
byte predicates, and token parsing shares its character table. Priority
went from 112 ns to 90 ns, and Content-Digest went from 105 ns to 87 ns
on their single-line fixtures. After sharing the token68 character
table, the authentication rerun measured 74 ns for token68 and 266 ns
for the challenge list, down from 82 ns and 303 ns.

### Range requests and partial responses

The Range parser accepts mixed-case byte units and canonicalizes them
to `bytes` when rendering. Extension units retain their received
spelling and visible-byte members. Content-Range retains the received
unit, requires a numeric length on the unsatisfied form, and supports
response and partial PUT use. These checks follow
[RFC 9110 §14](https://www.rfc-editor.org/rfc/rfc9110.html#section-14).
The public offset types use Word64. Values above its maximum fail
before conversion can wrap. The existing unsatisfied constructor can
still render legacy `*/*` when constructed explicitly.

The typed lookup and rendering codecs agreed with an independent
grammar on 97,917 cases. Property tests cover the full representable
offset range, and the 55,440-case bounded-decimal matrix passed again.
Simplified Core shows unboxed accumulators in the Range, Content-Range
and existing native-word decimal paths. The focused HTTP client suite
passed all 65 tests, including preservation of extension units through
the full Content-Range parser.

Sequential Criterion runs on GHC 9.10.3 and matching Go fixtures using
`strconv` measured the following. Go Range uses caller-provided stack
storage, and Go Content-Range returns a struct by value. Allocations
are rounded bytes per operation.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| Closed Range | 114 ns / 1,320 B | 43 ns / 544 B | 25–26 ns / 0 B |
| Three-member Range | 163 ns / 1,760 B | 90 ns / 904 B | 55 ns / 0 B |
| Satisfied Content-Range | 74 ns / 808 B | 36 ns / 424 B | 30 ns / 0 B |
| Unsatisfied Content-Range | 70 ns / 696 B | 28 ns / 376 B | 16 ns / 0 B |
| Content-Range render | 32 ns / 376 B | 30 ns / 376 B | 23 ns / 24 B |

The existing Content-Length and CORS Max-Age fixtures each still
allocate 272 bytes. Their final parse times were 18 and 20 ns.

### WebSocket handshake fields

Typed lookup checks the 16-byte key and 20-byte accept digest, including
padding and unused Base64 bits. The version parser accepts 0–255
without leading zeroes. A request can offer ordered, case-sensitive
subprotocols with each token appearing once. With
`lookupHeaderWith` and `withHeaderMessage`, request Version and response
Protocol each require one field line with one value. Response Version
combines advertised versions across field lines. Default lookup accepts
the list forms when the direction is unspecified.

Extension lines combine in order in both directions, per
[RFC 6455 erratum 3433](https://www.rfc-editor.org/errata/eid3433).
Quoted extension parameters must decode to HTTP tokens and render as
bare tokens. The Base64 parser uses fixed-length slices and the shared
Wireform decoder. Simplified GHC Core shows the shared HTTP token
scanner testing two precomputed ASCII bitmaps.

Typed lookup and rendering passed 52,782 independent WebSocket cases.
All 823 Hermes tests and 65 focused HTTP client tests passed. The shared
scanner also passed the 50,075 CORS and Resource Timing cases, 38,603
RFC 3986 cases and 2,029 browser URL fixtures.

Sequential Criterion runs and matching Go fixtures measured the
following. The Go parser puts its small typed results in caller
storage. Allocations are rounded bytes per operation.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| Key parse | 6.69 µs / 117,880 B | 41 ns / 568 B | 29–30 ns / 0 B |
| Accept parse | 7.99 µs / 137,400 B | 44 ns / 568 B | 26–27 ns / 0 B |
| Key render | 62 ns / 744 B | 16 ns / 200 B | 18–19 ns / 24 B |
| Version parse | 38 ns / 528 B | 22–25 ns / 336 B | 10.5–10.8 ns / 0 B |
| Protocol offers parse | 82 ns / 656 B | 41–43 ns / 496 B | 18.8–19.1 ns / 0 B |
| Extension parameters parse | 354 ns / 1,984 B | 94–101 ns / 744 B | 61–62 ns / 0 B |

Version and protocol offers exceeded twice the Go fixture time at this
checkpoint. See the [follow-up](#websocket-version-and-offers-follow-up).
Core shows one short-text allocation per retained token. The shared
token scanner also reduced single-line CORS Allow-Headers from 115 to
54 ns and authentication challenges from 288 to 187 ns, with the
same per-operation allocations on those fixtures.

### W3C Trace Context

The [Trace Context Level 1 recommendation](https://www.w3.org/TR/trace-context-1/)
defines a fixed prefix for `traceparent`. The typed decoder rejects
version `ff`, uppercase hex, repeated field lines and all-zero
identifiers. Version `00` consumes exactly the known layout. Later
versions retain an opaque suffix following an extension dash and
re-emit it for forwarding.

`tracestate` counts the 32-member limit across physical field lines,
including blank positions in the wire list. Blank positions disappear
from the canonical rendering. A present field with no entries is
`TracestateEmpty`, and `tracestateMemberList` reads both constructors.
The typed lookup keeps leading and internal value spaces, checks the key
and value lengths, and preserves the original member order.

The full Hermes suite passed 827 tests, the focused HTTP client suite
passed 65, and 96,396 independent parse/render cases passed. The latter
include all version and flag byte combinations, every replacement byte
in the fixed `traceparent` layout, and truncated slices at different
alignments. Simplified Core shows unboxed identifier scans with bounded
eight-byte reads. The tracestate value scanner uses the same read width
for printable groups and a byte scan around delimiters and at the end.

Criterion on GHC 9.10.3 and matching Go fixtures measured the following.
The Go parser retains strings as slices of its input and stores list
entries in caller storage. Allocations are rounded bytes per operation.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| Traceparent parse | 1.37 µs / 12,744 B | 44 ns / 496 B | 30–31 ns / 0 B |
| Tracestate parse | 18.83 µs / 319,304 B | 85 ns / 904 B | 37–39 ns / 0 B |
| Traceparent render | 39 ns / 520 B | 42 ns / 488 B | 19–20 ns / 64 B |

At that checkpoint tracestate parsing and traceparent rendering took
more than twice the matching Go fixture time.

### HSTS, HPKP and Expect-CT

The typed decoders reject case-insensitive duplicate directives. HSTS and
both HPKP fields use the first field line, as specified by
[RFC 6797 §8.1](https://www.rfc-editor.org/rfc/rfc6797.html#section-8.1)
and [RFC 7469 §2.3.1](https://www.rfc-editor.org/rfc/rfc7469.html#section-2.3.1).
[Expect-CT](https://www.rfc-editor.org/rfc/rfc9163.html#section-2.1)
combines directives across field lines and checks their uniqueness in the
combined list. All four decoders handle quoted-pair escapes in delta-seconds
and saturate values above the representable range.

The HPKP codecs keep recognized pins in wire order, validate canonical Base64
for 32-byte SHA-256 fingerprints, and ignore unsupported hash algorithms.
HPKP Report-Only accepts an omitted age as zero in its existing record type
and omits zero on rendering when another directive is present. Expect-CT
checks that report destinations are absolute URIs without fragments.

All 831 Hermes tests, 65 focused HTTP client tests, and 53,896 independent
typed parse/render cases passed. GHC Core shows the decimal scanner
accumulating in an unboxed word and the list parser retaining boxed
directives before assembling the typed record.

Sequential Criterion runs and matching Go fixtures measured the following
on GHC 9.10.3 and Go 1.26.10. Go retains strings as slices of the input and
stores pins in caller storage. Allocations are rounded bytes per operation.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| HSTS | 514 ns / 7,040 B | 99 ns / 800 B | 94–96 ns / 0 B |
| Expect-CT | 849 ns / 9,936 B | 161 ns / 992 B | 211–214 ns / 144 B |
| Public-Key-Pins | 2.32 µs / 25,439 B | 204 ns / 1,848 B | 284–286 ns / 0 B |
| Public-Key-Pins-Report-Only | 2.57 µs / 28,383 B | 316 ns / 2,280 B | 408–409 ns / 144 B |

### Forwarding fields and hop counts

[RFC 7239](https://www.rfc-editor.org/rfc/rfc7239.html#section-4)
defines case-insensitive parameter names unique to each hop. Hermes
validates nodes, host authorities and URI scheme names, preserving the
parameter spelling, quote form and hop order. Quoted extensions preserve
opaque HTTP bytes. Empty comma positions are skipped. A semicolon-only
hop is represented as an empty element and renders as a single semicolon.

X-Forwarded-For retains visible ASCII node identifiers from all physical
field lines. X-Forwarded-Host validates every authority in its single
field line and retains the original spelling of the chain.
X-Forwarded-Proto recognizes HTTP and HTTPS case-insensitively, and
X-Forwarded-Port accepts decimal ports through 65535. X-Real-IP accepts
IPv4 and IPv6. The shared IPv4 grammar rejects multi-digit octets with
leading zeroes, including embedded IPv4 tails. Max-Forwards rejects
values above the existing `Int` representation. Via accepts a port
after a pseudonym token under [RFC 9110 §7.6.3](https://www.rfc-editor.org/rfc/rfc9110.html#section-7.6.3).

All 840 Hermes tests, 65 focused HTTP client tests and 75,680
independent forwarding parse/render cases passed. The shared address
parser and bracketed Host agreed with Go netip on 72,886 inputs. The
shared list grammar also passed the 53,896 transport-policy parse/render
cases.

Sequential Criterion runs measured the following on GHC 9.10.3.
Matching Go 1.26.10 parse fixtures preserve strings as input slices
and put list entries in caller storage. Go uses netip for IP addresses,
net/url for host authorities, and strconv for the decimal fixtures.
Allocations are rounded bytes per operation.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| Forwarded | 480 ns / 4,824 B | 311 ns / 2,944 B | 163–164 ns / 0 B |
| X-Forwarded-For | 87 ns / 768 B | 71 ns / 592 B | 34 ns / 0 B |
| X-Forwarded-Host | 42 ns / 544 B | 62 ns / 376 B | 123–124 ns / 168 B |
| X-Forwarded-Port | 39 ns / 504 B | 20 ns / 272 B | 5.6 ns / 0 B |
| X-Forwarded-Proto | 50 ns / 536 B | 22 ns / 272 B | 7.4 ns / 0 B |
| X-Real-IP | 353 ns / 3,144 B | 34 ns / 384 B | 13 ns / 0 B |
| Max-Forwards | 39 ns / 504 B | 19 ns / 272 B | 4.8–4.9 ns / 0 B |
| Via (ordinary host and port) | 112 ns / 624 B | 108 ns / 624 B | |

X-Forwarded-Host now does authority validation on a path that previously
copied the field bytes. Simplified Core shows the bounded decimal scans
running on unboxed words. The typed field runner still contributes
boxing overhead on the smallest fixtures. Forwarded retains intermediate
parsed pairs before checking names and assembling its result. At this
checkpoint X-Forwarded-For, X-Forwarded-Port, X-Forwarded-Proto, X-Real-IP
and Max-Forwards exceeded twice the corresponding Go fixture time.

### WebDAV core and search

The [RFC 4918 condition grammar](https://www.rfc-editor.org/rfc/rfc4918.html#section-10.4.2)
is available through `ifConditionLists`. Tagged and untagged modes use
separate constructors. A condition retains its negation, state token or
entity-tag value. Parsed fields keep their bytes for forwarding, including
ETag octets that are invalid UTF-8. `fromIfConditionLists` renders a tree
with canonical whitespace. The existing `If` text constructor and accessor
remain available.

Destination accepts an absolute URI or a path-absolute with an optional
query. Lock-Token, DAV extensions, and [DASL](https://www.rfc-editor.org/rfc/rfc5323.html#section-3.2)
share the bracketed absolute-URI codec. DAV and Lock-Token have request and
response direction metadata. DAV can represent an explicitly empty field.
Timeout reads finite values through 4294967295 seconds and preserves the
preference order across lines.

All 847 Hermes tests, 65 focused HTTP client tests, 38,200 independent
WebDAV cases, and 38,603 RFC 3986 Location and Referer cases passed.
The WebDAV corpus checks original and canonical If rendering, every ETag
octet, mixed list modes, URI delimiters, repeated lines, and numeric bounds.

Sequential Criterion runs on GHC 9.10.3 measured the following. The Go
1.26.10 fixtures use net/url for URI validation, retain strings as input
slices and store parsed lists in caller-provided space. Allocations are
rounded bytes per operation.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| If parse and read wire text | 42 ns / 584 B | 194 ns / 1,392 B | |
| If parse and read condition tree | | 180 ns / 1,360 B | 178–180 ns / 288 B |
| If render condition tree | | 231 ns / 1,712 B | 23 ns / 64 B |
| Destination parse | 41 ns / 568 B | 89 ns / 384 B | 153–154 ns / 176 B |
| Lock-Token parse | 59 ns / 592 B | 72 ns / 392 B | 74 ns / 144 B |
| Depth parse | 38 ns / 472 B | 8 ns / 48 B | 6.4 ns / 0 B |
| Overwrite parse | 34 ns / 456 B | 7 ns / 32 B | 4.7 ns / 0 B |
| Timeout parse | 61 ns / 672 B | 33 ns / 336 B | 26 ns / 0 B |
| DAV parse | 94 ns / 856 B | 106 ns / 576 B | 140–141 ns / 144 B |
| DASL parse | 41 ns / 552 B | 51 ns / 464 B | 73 ns / 152 B |

The old If and Destination fixtures copied field text without parsing its
grammar. Simplified Core shows the Timeout decimal scan accumulating in an
unboxed word. At this checkpoint the If tree renderer paid for composing
per-condition builders and converting each compact fragment to a regular
byte string.

### Compact header rendering

The shared builder copies compact byte strings directly into the active
buffer when they fit. Larger fragments follow the existing insertion
path. The canonical WebDAV If renderer uses the same grammar traversal
to count and copy bytes, including opaque entity tags, so it can fill
one exactly sized output buffer.

The same GHC 9.10.3 build was measured before and after with sequential
Criterion runs. These are representative strict-rendering fixtures.
Allocations are rounded bytes per operation.

| Fixture | Previous Hermes | Current Hermes |
| --- | ---: | ---: |
| Accept | 255 ns / 2,287 B | 194 ns / 1,712 B |
| Content-Type | 97 ns / 1,000 B | 71 ns / 712 B |
| Content-Disposition | 165 ns / 1,440 B | 127 ns / 1,040 B |
| Signature-Input | 540 ns / 4,351 B | 487 ns / 4,063 B |
| Server | 90 ns / 808 B | 50 ns / 392 B |
| WebDAV If condition tree | 219 ns / 1,711 B | 89 ns / 272 B |
| Traceparent | 41 ns / 488 B | 23 ns / 280 B |
| Location | 27 ns / 392 B | 14 ns / 264 B |
| Cookie | 107 ns / 855 B | 46 ns / 280 B |
| Set-Cookie | 72 ns / 656 B | 42 ns / 368 B |

At this checkpoint WebDAV If tree rendering took roughly four times
the 23 ns Go fixture. Simplified Core shows direct byte-array copies into the
output pointer for both the shared compact builder and the If renderer.

All 166 Wireform core tests passed with host shared-memory access,
including strict, fixed-size, lazy and ring-buffer boundary checks.
All 847 Hermes tests and 65 focused HTTP client tests passed. The
WebDAV, forwarding, transport-policy, Trace Context and WebSocket
corpora also passed all 316,954 typed parse/render cases.

### WebDAV extension fields

[Label](https://www.rfc-editor.org/rfc/rfc3253.html#section-8.3) checks
percent escapes and decoded UTF-8. The typed value retains the original
case and spelling. `labelText` exposes decoded text, and `labelFromText`
constructs a URL-escaped value. A literal plus sign remains a plus sign.

[Position](https://www.rfc-editor.org/rfc/rfc3648.html#section-6.1)
validates an escaped path segment and renders its keyword in lowercase.
[Ordering-Type](https://www.rfc-editor.org/rfc/rfc3648.html#section-5.1)
requires a fragmentless absolute URI.
[Redirect-Ref and Apply-To-Redirect-Ref](https://www.rfc-editor.org/rfc/rfc4437.html#section-12)
accept a URI-reference and a case-insensitive flag respectively.
Legacy [Status-URI](https://www.rfc-editor.org/rfc/rfc2518.html#section-9.7)
retains ordered pairs of 100–599 status codes and bracketed absolute URIs.

All 853 Hermes tests, 65 focused HTTP client tests, and 121,016
independent extension cases passed. The corpus includes the RFC 3986
reference fixtures, all three-digit code values, and malformed escaped
UTF-8. GHC Core shows the Position scan using two ASCII bitmaps.

Sequential Criterion measurements on GHC 9.10.3 and corresponding
Go 1.26.10 fixtures measured the following. Go keeps slices of the
input and uses net/url for URI and percent decoding. Allocations are
rounded bytes per operation.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| Label, plain | 46 ns / 544 B | 30 ns / 344 B | 21 ns / 0 B |
| Label, escaped | 52 ns / 560 B | 79 ns / 632 B | 71–72 ns / 24 B |
| Position | 6.56 µs / 102,224 B | 44 ns / 376 B | 31 ns / 0 B |
| Ordering-Type | 40 ns / 544 B | 35 ns / 360 B | 64–66 ns / 152 B |
| Redirect-Ref | 42 ns / 568 B | 65 ns / 384 B | 160–162 ns / 192 B |
| Apply-To-Redirect-Ref | 36 ns / 456 B | 6 ns / 32 B | 4.3 ns / 0 B |
| Status-URI | 95 ns / 912 B | 142 ns / 688 B | 206–208 ns / 288 B |

The escaped Label, Redirect-Ref and Status-URI fixtures now perform
validation that the previous versions skipped. A second Position run
measured 6.49 µs before and 44 ns after.

### CalDAV scheduling and attachments

[Schedule-Tag and If-Schedule-Tag-Match](https://www.rfc-editor.org/rfc/rfc6638.html#section-8.2)
use the quoted-string production referenced by RFC 6638. Both preserve
the original body, including quoted pairs and opaque octets, so a
response tag can be sent back as a precondition. The byte accessor
supports exact comparisons, and the original text interface remains
available.

[Cal-Managed-ID](https://www.rfc-editor.org/rfc/rfc8607.html#section-5.1)
is a singleton response field. It checks the iCalendar paramtext
delimiters and validates UTF-8. [Schedule-Reply](https://www.rfc-editor.org/rfc/rfc6638.html#section-8.1)
and [CalDAV-Timezones](https://www.rfc-editor.org/rfc/rfc7809.html#section-7.1)
use the same case-insensitive flag codec as WebDAV Overwrite and
Apply-To-Redirect-Ref and render uppercase values.

All 857 Hermes tests, 65 focused HTTP client tests, and 92,679
independent CalDAV and WebDAV flag cases passed. The corpus checks
every octet in literal and escaped scheduling tag positions, every
two-byte managed ID, and repeated field lines. The WebDAV extension,
forwarding and transport-policy corpora added 250,592 passing cases
against the final shared parser.

Sequential Criterion runs on GHC 9.10.3 and matching Go 1.26.10
fixtures measured the following. Go retains the input string slices.
Allocations are rounded bytes per operation.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| Schedule-Tag | 14.39 µs / 230,216 B | 21 ns / 112 B | 16–17 ns / 0 B |
| If-Schedule-Tag-Match | 13.83 µs / 230,216 B | 22 ns / 112 B | 16 ns / 0 B |
| Schedule-Reply | 32 ns / 456 B | 6.4 ns / 32 B | 3.6 ns / 0 B |
| CalDAV-Timezones | 32 ns / 456 B | 4.2 ns / 32 B | 3.3 ns / 0 B |
| Cal-Managed-ID | 37 ns / 544 B | 27 ns / 168 B | 14–15 ns / 0 B |

The plain scheduling tag path checks the original slice and copies
the body once. Simplified Core shows raw address indexing in the
scanner and a single copy into the retained compact byte array.

### OASIS Repeatable Requests

The [repeatability fields](https://docs.oasis-open.org/odata/repeatable-requests/v1.0/cs01/repeatable-requests-v1.0-cs01.html#sec_HeaderFields)
are singletons. Both identifiers preserve the received case and accept
opaque HTTP tokens in addition to UUIDs. Repeatability-Result accepts
every ASCII case spelling of either disposition and emits lowercase.
Repeatability-First-Sent emits IMF-fixdate and accepts the two legacy
formats on receipt, following [RFC 9110](https://www.rfc-editor.org/rfc/rfc9110.html#section-5.6.7).

Typed header map tests cover every octet in the middle of an identifier,
with and without surrounding OWS, and all 512 case spellings of the result
literals. They also check combined values, duplicate lines, and canonical
output through `setHeader`. All 864 Hermes tests and 65 focused HTTP client
tests passed.

Sequential Criterion runs used GHC 9.10.3. The Go 1.26.10 identifier
fixtures validate the same token grammar and copy the result into an owned
string, matching Hermes's compact `ShortText` ownership. Go also measured
12–13 ns with borrowed strings. Allocations are bytes per operation.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| Request-ID | 59 ns / 568 B | 31 ns / 104 B | 23–24 ns / 48 B |
| Client-ID | 52 ns / 568 B | 33 ns / 104 B | 23–24 ns / 48 B |
| First-Sent parse | 134 ns / 1,000 B | 127 ns / 1,000 B | 81–83 ns / 0 B |
| First-Sent render | 32 ns / 360 B | 28 ns / 160 B | 73–77 ns / 32 B |
| Result, canonical | 34 ns / 456 B | 6 ns / 32 B | 6 ns / 0 B |

The bare identifier path validates before copying the bytes into compact
storage. Canonical result values bypass parser setup, and typed First-Sent
rendering uses Date's fixed-size output path. First-Sent parsing still pays
the shared HTTP-date decoder's allocation cost.

### OData version and isolation

`OData-Version` parses the `4.0` family through `4.09` and keeps the
leading fractional zero in each three-digit version. `OData-MaxVersion`
retains additional fractional zeros in a separate constructor, so `4.01`
and `4.1` remain different decimal values. The bare constructor keeps
its ordinary decimal rendering. Both numeric components of a maximum
version are checked against `Word` before conversion. [OASIS header grammar](https://docs.oasis-open.org/odata/odata/v4.01/os/abnf/odata-abnf-construction-rules.txt), [protocol versioning](https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part1-protocol.html#sec_ProtocolVersioning).

`Isolation` and `OData-Isolation` accept all ASCII case spellings of
`snapshot` and emit lowercase. Other valid tokens retain their spelling.
Typed header map tests cover field whitespace, repeated lines, zero-padded
fractions, overflow in each component, and extension tokens with a
`snapshot` prefix. All 871 Hermes tests, 65 focused HTTP client tests,
and 25,312 vectors from an independent Python grammar and integer oracle
passed.

Sequential Criterion results on GHC 9.10.3 used the typed field codec.
The Go 1.26.10 fixtures used the same version and isolation literals and
kept the fractional zero count when parsing the extended version.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| Version `4.01` parse | 37 ns / 544 B | 7.1 ns / 48 B | 3.6 ns / 0 B |
| Max-Version `4.01` parse | 38 ns / 544 B | 7.3 ns / 48 B | 3.3–3.8 ns / 0 B |
| Max-Version `12.0001234567890` parse |  | 36 ns / 400 B | 28 ns / 0 B |
| Version `4.01` render | 13 ns / 280 B | 4.5 ns / 32 B | 1.7 ns / 0 B |
| Max-Version `4.01` render | 13 ns / 280 B | 5.0 ns / 32 B | 1.7 ns / 0 B |
| Isolation `snapshot` parse | 47 ns / 520 B | 6.1 ns / 32 B | 6.2 ns / 0 B |
| OData-Isolation `snapshot` parse | 44 ns / 520 B | 6.4 ns / 32 B | 6.2 ns / 0 B |

Simplified Core shows unboxed digit accumulation for extended maximum
versions and prebuilt field bytes for canonical outputs. The previous
version fixtures rendered `4.1`. The Go renderer returns a constant
string, and the Criterion fixture forces the strict `ByteString` result.

### OData entity references

`OData-EntityId` accepts nonempty absolute and relative IRI references.
The parser preserves the original UTF-8 and percent escapes and limits
raw Unicode to the appropriate components. Raw private-use code points
are accepted in queries. Rendering percent-encodes non-ASCII UTF-8 octets
and keeps existing escapes verbatim. [OData entity identifiers](https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part1-protocol.html), [RFC 3987](https://www.rfc-editor.org/rfc/rfc3987.html#section-2.2).

All 876 Hermes tests, 65 focused HTTP client tests, and 112,081
independent IRI cases passed. The corpus checks all two-octet combinations
within a path and the Unicode range boundaries across different components.
All 38,603 RFC 3986 fixtures were also rerun through Location and Referer
after sharing the URI and IRI reference structure.

Sequential Criterion runs on GHC 9.10.3 measured the typed codec. The
previous parser copied field contents without checking syntax. Go 1.26.10
uses `net/url.Parse`, which constructs URL components, and a separate
UTF-8 check. The renderer is compared with Go's owned string output.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| Absolute parse | 40 ns / 576 B | 97 ns / 456 B | 175–181 ns / 176 B |
| Relative parse | 39 ns / 568 B | 88 ns / 448 B | 183–210 ns / 216 B |
| Unicode parse | 39 ns / 568 B | 112 ns / 544 B | 155–173 ns / 168 B |
| ASCII render | 13 ns / 264 B | 27 ns / 160 B | 25–28 ns / 48 B |
| Unicode render | 14 ns / 264 B | 46 ns / 192 B | 119–120 ns / 48 B |

The previous Unicode renderer emitted raw UTF-8. Simplified Core for
the current renderer shows bounded eight-byte reads for the size check
and unboxed source and destination offsets in the escape loop.

### Content-ID and Differential-ID

`Content-ID` accepts classic MIME message IDs and OData multipart batch IDs.
The MIME grammar includes dot-separated atoms, domain literals, quoted local
parts, and comments. Batch IDs contain one or more RFC 3986 unreserved
characters. [MIME Content-ID](https://www.rfc-editor.org/rfc/rfc2045.html#section-7), [message ID syntax](https://www.rfc-editor.org/rfc/rfc5322.html#section-3.6.4), [OData header grammar](https://docs.oasis-open.org/odata/odata/v4.01/os/abnf/odata-abnf-construction-rules.txt).

Both fields also accept a DRP identifier made up of comma-separated absolute
URIs. A raw comma separates members, while `%2C` stays inside a member. The
parser checks each member within its input span and retains the entire
identifier in one copy. The typed header map strips surrounding field OWS
and rejects repeated lines. [W3C DRP](https://www.w3.org/TR/NOTE-drp-19970825).

All 882 Hermes tests, 65 focused HTTP client tests, and 114,593 independent
wire cases passed. The corpus covers URI references and octet substitutions
in representative identifier forms. Permanent tests cover nested MIME
comments and repeated field lines.

Sequential Criterion runs on GHC 9.10.3 measured the typed codec. The Go
1.26.10 fixtures use `net/mail.ParseAddress` and `net/url.Parse` and copy
the accepted wire spelling into an owned string.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| OData batch ID | 48 ns / 552 B | 38 ns / 120 B | 22–24 ns / 24 B |
| MIME message ID | 39 ns / 560 B | 102 ns / 456 B | 213–227 ns / 192 B |
| Single DRP ID | 47 ns / 560 B | 88 ns / 440 B | 76–77 ns / 176 B |
| Composite DRP ID | 42 ns / 576 B | 148 ns / 472 B | 179–194 ns / 336 B |
| Differential-ID | 42 ns / 560 B | 71 ns / 440 B | 75–77 ns / 176 B |

The previous codec copied the input without validating it. Simplified Core
for the current bare batch ID path shows a byte scan and one copy into
compact storage.

### Content-Base and URI

`Content-Base` requires a single fragmentless absolute URI and rejects
repeated field lines. The legacy `URI` header requires a bracketed member,
permits absolute and relative references, and preserves an empty reference
inside `<>`. RFC 2068 list rules allow empty comma-separated slots and
combine repeated fields in their received order. Commas inside brackets
remain part of the reference. Serialization uses one canonical field line.
Both codecs share RFC 3986 URI validation. [Content-Base](https://www.rfc-editor.org/rfc/rfc2068.html#section-14.11), [URI](https://www.rfc-editor.org/rfc/rfc2068.html#section-19.6.2.5), [RFC 3986](https://www.rfc-editor.org/rfc/rfc3986.html#section-4).

All 887 Hermes tests, 65 focused HTTP client tests, and 188,974 independent
wire cases passed. The independent cases check URI syntax, round trips,
repeated lines, and octet substitutions in both fields.

Sequential Criterion runs used GHC 9.10.3. The Go 1.26.10 parser fixtures
use `net/url.Parse`, which constructs URL components, and retain owned
strings for the accepted wire values. The previous URI list parser only
read the first field line and skipped validation inside the brackets.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| Content-Base parse | 38 ns / 576 B | 86 ns / 392 B | 230–239 ns / 248 B |
| Content-Base render | 15 ns / 264 B | 14 ns / 160 B | 11–13 ns / 48 B |
| URI list parse | 69 ns / 680 B | 110 ns / 512 B | 310–311 ns / 400 B |
| URI repeated-line parse | 52 ns / 576 B | 134 ns / 840 B | 294–316 ns / 400 B |
| URI list render | 83 ns / 968 B | 25 ns / 280 B | 26–27 ns / 64 B |

### Content-Version and Derived-From

Both version fields carry an HTTP quoted string. Hermes decodes quoted pairs,
retains the decoded tag bytes, and emits escapes for quote and backslash
octets. The byte accessors support forwarding a received tag as a
`Derived-From` value. The text constructors encode UTF-8, and the text views
use Latin-1 for octet sequences that are invalid UTF-8. The codec accepts
field OWS and rejects repeated lines. [RFC 2068 version fields](https://www.rfc-editor.org/rfc/rfc2068.html#section-19.6.2.2), [RFC 9110 quoted strings](https://www.rfc-editor.org/rfc/rfc9110.html#section-5.6.4).

All 891 Hermes tests, 65 focused HTTP client tests, and 262,144 independent
wire cases passed. The independent corpus checks every octet pair under
each combination of literal and escaped spelling. Permanent typed-header
cases also verify cross-field copying, canonical quoting, and cardinality.

Sequential Criterion runs on GHC 9.10.3 used the typed codecs. The Go
1.26.10 fixtures decode the same HTTP grammar and own the returned string.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| Plain Content-Version parse | 347 ns / 4,152 B | 24 ns / 88 B | 23–24 ns / 24 B |
| Plain Content-Version parse with text access | 347 ns / 4,152 B | 29 ns / 120 B |  |
| Plain Derived-From parse | 355 ns / 4,152 B | 24 ns / 88 B | 23–24 ns / 24 B |
| Quoted-pair parse | 311 ns / 3,624 B | 60 ns / 552 B | 32–34 ns / 24 B |
| Plain render | 36 ns / 448 B | 24 ns / 184 B | 24–25 ns / 24 B |
| Escaped render | 47 ns / 576 B | 40 ns / 184 B | 66 ns / 24 B |

Simplified Core shows unboxed input and output offsets in the shared
unquoting loop. The plain parse checks the input slice and copies it into
compact storage. The renderer sizes its output before writing the field.
The Schedule-Tag and If-Schedule-Tag-Match fixtures continued to allocate
112 bytes per parse after sharing the plain quoted-string path.

### Legacy HTML metadata

HTML 4 gives the final HTTP field line precedence for `Content-Script-Type`,
`Content-Style-Type`, and `Default-Style`. The media fields validate the
selected line with the shared Content-Type grammar, expose parsed media
types and parameters, and retain the received spelling within field OWS.
An invalid final media type is reported as a parse failure. [HTML 4 scripts](https://www.w3.org/TR/html401/interact/scripts.html#h-18.2.2), [HTML 4 style types](https://www.w3.org/TR/html401/present/styles.html#h-14.2.1).

`Default-Style` supports quoted and bare titles on receipt. The selected
title is retained as opaque bytes and rendered as an HTTP quoted string.
Spaces, commas, and escaped bytes within the title remain part of the value.
[HTML 4 preferred style sheets](https://www.w3.org/TR/html401/present/styles.html#h-14.3.2).

Typed tests check mixed-case field names, final-line precedence, quoted
media parameters, and opaque title bytes. The independent grammar corpus
mutates every octet position of representative field values and checks
the result with an earlier field line present.

All 896 Hermes tests, 65 focused HTTP client tests, and 73,728 independent
wire cases passed. The original text record constructors and record updates
also compiled against the new representations.

Sequential Criterion runs on GHC 9.10.3 used typed header codecs. Go
1.26.10 uses `mime.ParseMediaType` for media fields and retains owned
strings. The previous media codecs copied the field without validating it.
The previous Default-Style codec treated quotes as part of the title.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| Content-Script-Type parse | 43 ns / 544 B | 70 ns / 688 B | 55–59 ns / 64 B |
| Content-Style-Type parse | 38 ns / 552 B | 89 ns / 984 B | 127–128 ns / 360 B |
| Default-Style parse | 39 ns / 544 B | 22 ns / 112 B | 20 ns / 16 B |
| Default-Style render | 14 ns / 264 B | 21 ns / 168 B | 22–25 ns / 16 B |

The shared media-name scanner uses two ASCII membership masks. Simplified
Core shows byte reads and bit tests inside the scan. The same change moved
the Accept parse fixture from 214 to 179 ns and the Content-Type parse
fixture from 166 to 124 ns. Those fixtures allocated 96 and 48 additional
bytes per operation, respectively.

### OSLC configuration and version fields

`Configuration-Context` accepts an RFC 3986 URI with a scheme and an
optional query and fragment. The request-only typed codec retains the URI
spelling. The angle-bracket syntax is defined for the `oslc_config.context`
query parameter. [OSLC Configuration Resources §4](https://docs.oasis-open-projects.org/oslc-op/config/v1.0/config-resources.html), [RFC 3986 §3](https://www.rfc-editor.org/rfc/rfc3986.html#section-3).

`OSLC-Core-Version` accepts two nonempty decimal components separated by a
dot, with a major version of at least 2. Leading zeroes remain on the wire,
and the numeric accessor returns both components as `Natural`. Both fields
accept surrounding HTTP field whitespace and reject repeated lines.
[OSLC Core 3.0 §4.2.2](https://docs.oasis-open-projects.org/oslc-op/core/v3.0/os/oslc-core.html).

All 901 Hermes tests and 65 focused HTTP client tests passed. An independent
92,726-case OSLC corpus covers URI spelling and field whitespace, byte
substitutions in version fields, and versions with up to 128 significant
digits per component. The shared URI grammar also passed all 38,603 cases
in the existing RFC 3986 reference corpus.

Sequential Criterion runs on GHC 9.10.3 used the typed field codecs. The
Go 1.26.10 harness parses individual field values, retains owned strings,
and uses `net/url.Parse` to build the context URL. The previous Hermes
codecs copied the input without checking either field's grammar.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| Core version `3.0` parse | 38.6 ns / 536 B | 5.3 ns / 48 B | 3.2–3.4 ns / 0 B |
| Padded version parse | 37.5 ns / 544 B | 28 ns / 344 B | 19–20 ns / 16 B |
| Padded version parse and numeric access | | 50 ns / 528 B | 50–51 ns / 32 B |
| Configuration context parse | 37.6 ns / 584 B | 88 ns / 400 B | 162–171 ns / 208 B |
| Configuration context render | 14.2 ns / 264 B | 13.1 ns / 176 B | 11.5–12.4 ns / 64 B |

The first version fast path compared two three-byte literals and incurred
one or two `memcmp` calls. Simplified Core for the current path shows a
length check, direct byte reads, and shared results for `2.0` and `3.0`.
The generic path scans decimal bytes with unboxed offsets and copies the
accepted spelling once into compact storage.

### Browser isolation policy fields

The opener and embedder codecs and their report-only variants accept the
published case-sensitive policy tokens as Structured Fields Items. They
validate every parameter and retain the final `report-to` value when it
is a Structured Fields String. A later bare flag or non-string value
clears a previously supplied endpoint. Rendering escapes the selected
endpoint and preserves an explicitly empty string. [HTML opener policy](https://html.spec.whatwg.org/multipage/browsers.html#cross-origin-opener-policies), [HTML embedder policy](https://html.spec.whatwg.org/multipage/browsers.html#coep), [RFC 9651 parameter parsing](https://www.rfc-editor.org/rfc/rfc9651.html#section-4.2.3.2).

Origin-Agent-Cluster validates a Boolean Item and ignores its parameters.
Resource policy accepts exactly the three Fetch tokens. All six typed
codecs reject repeated field lines and comma-joined values. [HTML document initialization](https://html.spec.whatwg.org/multipage/document-lifecycle.html#initialise-the-document-object), [Fetch resource policy](https://fetch.spec.whatwg.org/#cross-origin-resource-policy-header).

All 908 Hermes tests and 65 focused HTTP client tests passed. A differential
run against Go `httpsfv` v1.1.2 with matching policy projection agreed on
78,525 wire cases, including 2,304 adapted HTTP Working Group cases. The
comparison covered acceptance and the canonical field value rendered from
typed lookup. [HTTP WG Structured Fields tests](https://github.com/httpwg/structured-field-tests), [Go Structured Fields library](https://github.com/dunglas/httpsfv/tree/v1.1.2).

Sequential Criterion runs used GHC 9.10.3. The Go 1.26.10 fixtures use
`httpsfv` for the Structured Fields Items and compare the exact strings for
resource policy. The Go Item parser materializes the parameter map. The
Hermes parser tracks the selected parameter at the byte cursor. Simplified
Core shows a parameter loop with unboxed cursor arithmetic and no
intermediate parameter list.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| Embedder policy parse | 37 ns / 480 B | 21 ns / 280 B | 55–56 ns / 96 B |
| Embedder policy with endpoint parse | 225 ns / 2,448 B | 44 ns / 400 B | 159–167 ns / 424 B |
| Opener policy parse | 40 ns / 480 B | 23 ns / 280 B | 60–63 ns / 96 B |
| Opener policy with endpoint parse | 191 ns / 2,176 B | 44 ns / 400 B | 156–159 ns / 424 B |
| Resource policy parse | 38 ns / 456 B | 6.5 ns / 32 B | 4.2–4.3 ns / 0 B |
| Origin-Agent-Cluster parse | 33 ns / 456 B | 5.8 ns / 48 B | 39 ns / 80 B |
| Embedder policy render | 16 ns / 280 B | 4.3 ns / 32 B | 53–56 ns / 72 B |
| Embedder policy with endpoint render | 37 ns / 432 B | 37 ns / 464 B | 103–105 ns / 168 B |

### Browser framing, sniffing, and site-data fields

The RFC 7034 codec accepts case-insensitive `DENY`, `SAMEORIGIN`, and
`ALLOW-FROM` directives and rejects repeated field lines. The legacy
directive requires a serialized origin and accepts a valid slash-led URI
suffix, as in the RFC example. The typed origin accessor ignores the
suffix. Rendering preserves its spelling and normalizes directive casing
and field whitespace. [RFC 7034 §2](https://www.rfc-editor.org/rfc/rfc7034.html#section-2).

The typed `X-Content-Type-Options` codec reads one token and retains
extension tokens. `hasNoSniff` implements Fetch’s first-member test for
received headers, including repeated lines and comma-separated values.
An empty or quoted first member does not match. [Fetch sniffing policy](https://fetch.spec.whatwg.org/#x-content-type-options-header).

`Clear-Site-Data` combines the quoted directive list in wire order across
field lines, skips empty slots, and requires at least one member. The
case-sensitive `clientHints` value has its own constructor. Canonically
quoted UTF-8 extensions use the existing text constructor. Other valid
extensions preserve their exact escapes and opaque bytes. Escaped spellings
of registered names remain extensions on render. [Clear Site Data §3.1](https://w3c.github.io/webappsec-clear-site-data/#header), [RFC 9110 field lists](https://www.rfc-editor.org/rfc/rfc9110.html#section-5.6.1).

All 914 Hermes tests and 65 focused HTTP client tests passed. The
independent 95,556-case corpus checked typed lookup, canonical field
output, all ASCII case combinations for `nosniff`, projected legacy
origins, and mutated quoted data across physical field lines.

Sequential Criterion runs used GHC 9.10.3. The Go 1.26.10 fixtures use the
standard library for case folding and legacy URL parsing, and return
typed site-data directives from an independent quoted-list decoder.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| `nosniff` parse | 128 ns / 1,888 B | 5.5 ns / 32 B | 5.5–5.6 ns / 0 B |
| Mixed-case `nosniff` parse | 128 ns / 1,888 B | 5.6 ns / 32 B | 6.8–6.9 ns / 0 B |
| `DENY` parse | 100 ns / 1,312 B | 6.2 ns / 32 B | 5.3–5.4 ns / 0 B |
| `ALLOW-FROM` parse | 177 ns / 2,576 B | 101 ns / 656 B | 148–153 ns / 176 B |
| `DENY` render | 16 ns / 280 B | 4.4 ns / 32 B | |
| Three Clear-Site-Data directives | 630 ns / 7,391 B | 34 ns / 344 B | 96–97 ns / 168 B |

Simplified Core shows a length guard and masked byte reads for `nosniff`.
The fixed `DENY` renderer returns a shared byte string. Criterion includes
the cost of forcing the renderer’s typed result. Keeping the URI suffix
scanner out of line reduced the optimized X-Frame-Options object from
204 KiB to 151 KiB in the Core inspection build.

### Client hints, purpose, and transport signals

`Accept-CH` validates each advertised name as both a Structured Fields
Token and an HTTP field name. It validates member parameters, projects the
names in order across physical lines, and distinguishes an explicit empty
opt-in from an absent header. Repeated lines must each have at least one
member. [RFC 8942 §3.1](https://www.rfc-editor.org/rfc/rfc8942.html#section-3.1), [Client Hints Infrastructure](https://wicg.github.io/client-hints-infrastructure/#accept-ch).

`Sec-Purpose` accepts Fetch's bare token and the WICG Structured Fields List,
including Items with parameters and extension inner lists. It preserves
member order and the last value for each parameter. The prefetch accessor
reports `prerender` and `anonymous-client-ip` for each `prefetch` Item. A
present value other than Boolean false sets the corresponding flag. The
original `SecPurpose` pattern covers one bare token. `secPurposeMembers`
reads the full field. [Fetch Sec-Purpose](https://fetch.spec.whatwg.org/#sec-purpose-header), [WICG Sec-Purpose](https://wicg.github.io/nav-speculation/prefetch.html#sec-purpose-header).

`Capsule-Protocol` validates a singleton Boolean Item and ignores its
parameters after parsing. `capsuleProtocolInUse` is true only for a valid
true Item. `Early-Data` renders the only authored value, `1`. Typed receive
lookup returns that marker for any present field, including malformed and
repeated values, as the RFC requires. [RFC 9297 §3.4](https://www.rfc-editor.org/rfc/rfc9297.html#section-3.4), [RFC 8470 §5.1](https://www.rfc-editor.org/rfc/rfc8470.html#section-5.1).

All 917 Hermes tests and 65 focused HTTP client tests passed. The
differential run against Go `httpsfv` v1.1.2 with equivalent field-specific
rules matched all 99,734 vectors, including 2,304 adapted HTTP Working
Group cases. It checked typed acceptance, canonical rendering, and the
prefetch and capsule projections. [HTTP WG Structured Fields tests](https://github.com/httpwg/structured-field-tests), [Go Structured Fields library](https://github.com/dunglas/httpsfv/tree/v1.1.2).

Sequential Criterion runs used GHC 9.10.3. The Go 1.26.10 fixtures use
`httpsfv` to build full Structured Fields Items before projecting names or
flags. The Early-Data baseline in Go checks field presence.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| Three Accept-CH hints | 105 ns / 840 B | 76 ns / 640 B | 336–405 ns / 536 B |
| Accept-CH across two lines | 138 ns / 1,368 B | 95 ns / 944 B | 436–450 ns / 488 B |
| Accept-CH with parameters | | 94 ns / 736 B | 630–726 ns / 1,144 B |
| Bare `Sec-Purpose: prefetch` | 55 ns / 520 B | 6.2 ns / 32 B | 97–105 ns / 136 B |
| Sec-Purpose with parameters and projection | | 132 ns / 824 B | 210–250 ns / 472 B |
| Bare Sec-Purpose render | 14 ns / 264 B | 8.8 ns / 56 B | 45–46 ns / 64 B |
| Bare Capsule-Protocol | 34 ns / 456 B | 6.9 ns / 32 B | 35–39 ns / 80 B |
| Capsule-Protocol with parameters | | 39 ns / 336 B | 165–166 ns / 440 B |
| Early-Data lookup | 39 ns / 552 B | 3.1 ns / 32 B | 1.7 ns / 0 B |

Simplified Core shows a shared typed result for present Early-Data, and
direct cursor scans with compact token copies for Accept-CH. The parameter
skip path validates keys without constructing a parameter list.

### Privacy and data preferences

`Sec-GPC` asserts Global Privacy Control when at least one physical field
line carries `1` after HTTP field whitespace. `hasSecGPC` also handles
requests with only invalid lines as unasserted. Typed lookup reports a parse
failure if every present line is invalid. The rendered field has one line
carrying `1`. [W3C Global Privacy Control §3.3](https://www.w3.org/TR/gpc/).

`DNT` reads one preference digit and preserves the extension suffix defined
by the retired W3C draft. The suffix excludes quotes, commas, backslashes,
spaces and other octets outside visible ASCII. `dntWithExtension` validates
the same grammar for authored values. The original `DNT` constructor
pattern and Boolean accessor also work with the existing bare format.
[W3C Tracking Preference Expression §5.2](https://www.w3.org/TR/tracking-dnt/#dnt-header-field-for-http-requests).

`Save-Data` parses the Structured Fields List across repeated lines and
retains Token Items and parameters. `saveDataPreference` reports the final
token or absence when the list is empty. The existing `SaveData` pattern
covers a singleton bare token. [WICG Save Data API](https://wicg.github.io/savedata/).

All 920 Hermes tests and 65 focused HTTP client tests passed. The
independent Go corpus agreed with typed lookup, rendering and the preference
accessors on 73,786 vectors, including 768 adapted HTTP Working Group
vectors for Save-Data. [HTTP WG Structured Fields tests](https://github.com/httpwg/structured-field-tests), [Go Structured Fields library](https://github.com/dunglas/httpsfv/tree/v1.1.2).

Sequential Criterion runs used GHC 9.10.3. Go 1.26.10 uses byte-wise
validators for Sec-GPC and DNT and `httpsfv` for Save-Data.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| Bare Sec-GPC | 32 ns / 456 B | 4.2 ns / 32 B | 3.1 ns / 0 B |
| Sec-GPC with a late assertion | | 10.6 ns / 32 B | 9.0–9.1 ns / 0 B |
| Bare DNT | 35 ns / 456 B | 5.7 ns / 32 B | 3.9–4.0 ns / 0 B |
| DNT with extension | | 18.3 ns / 120 B | 16.3–16.6 ns / 16 B |
| Bare Save-Data | 40 ns / 536 B | 8.0 ns / 48 B | 86–89 ns / 136 B |
| Save-Data with parameters across lines | | 135 ns / 1,064 B | 339–359 ns / 944 B |
| Bare Save-Data render | 14 ns / 264 B | 9.0 ns / 56 B | 47–50 ns / 64 B |

Simplified Core shows direct byte reads and shared typed values for the
bare Sec-GPC and DNT fixtures. The DNT extension path scans the raw suffix
and copies the accepted bytes into compact storage. It allocates 120 B per
operation, down from 352 B in the initial parser-based implementation.

### Permissions-Policy

The codec retains the ordered Structured Fields Dictionary, including its
Item and Inner List forms, unknown features, and extension parameters.
Duplicate keys across multiple field lines use the final value in the
first key's position. The typed declaration view exposes a Token-valued
`report-to`, recognizes `self` and `*`, and validates quoted CSP scheme
and host sources. Valid source expressions include wildcard hosts and
ports, scheme-only expressions, and percent-escaped paths.
[Permissions Policy §5.2](https://www.w3.org/TR/permissions-policy/#structured-header-serialization), [CSP source grammar](https://www.w3.org/TR/CSP3/#grammardef-host-source), [RFC 9651 dictionary parsing](https://www.rfc-editor.org/rfc/rfc9651.html#section-4.2.2).

The projection leaves an unsupported individual Item out of an Inner List.
An unsupported top-level member value has no typed allowlist. A valid
empty Inner List yields an empty allowlist. `src` remains available in the
legacy view and is ignored in the HTTP declaration view. The original
record construction and updates compile, and an empty dictionary still
replaces an existing value through `setHeader`.
[Permissions Policy header and iframe rules](https://www.w3.org/TR/permissions-policy/#delivery).

All 922 Hermes tests and 65 focused HTTP client tests passed. The
differential run matched Go `httpsfv` v1.1.2 and an independent CSP source
grammar on 96,395 wire vectors, including 768 adapted HTTP Working Group
vectors. It checked typed lookup, rendering, allowlists, and reporting.
[HTTP WG Structured Fields tests](https://github.com/httpwg/structured-field-tests), [Go Structured Fields library](https://github.com/dunglas/httpsfv/tree/v1.1.2).

Sequential Criterion runs used GHC 9.10.3. The Go 1.26.10 fixtures use
`httpsfv` and a compiled Go regular expression for CSP source validation.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| Parse and project three feature allowlists | 511 ns / 5,392 B | 329 ns / 2,112 B | 813–815 ns / 1,144 B |
| Parse and project across two field lines | 560 ns / 5,839 B | 340 ns / 2,264 B | 833–916 ns / 1,208 B |
| Parse wildcard source and reporting endpoints | | 361 ns / 2,040 B | 919–932 ns / 1,600 B |
| Render three feature allowlists | 244 ns / 2,344 B | 82 ns / 496 B | 172–187 ns / 272 B |

The shared Structured Fields renderer now folds members directly into the
builder. Simplified Core shows a recursive writer on the source entries
with no intermediate list of rendered members or separators.

### Legacy browser controls

X-DNS-Prefetch-Control accepts `on` and `off` with ASCII case folding and
emits lowercase. X-Download-Options uses the documented `noopen` literal.
Both accept HTTP field whitespace and require one physical field line.
[Chromium's DNS-prefetch implementation](https://chromium.googlesource.com/chromium/src/+/4f8e062e9cfff4d035eccf696aa8f368881e22e4/third_party/blink/renderer/core/dom/document.cc), [Microsoft's IE8 Force Save documentation](https://learn.microsoft.com/en-us/archive/blogs/ie/ie8-security-part-v-comprehensive-protection#mime-handling-force-save).

X-Permitted-Cross-Domain-Policies represents Adobe's `none-this-response`
alone or alongside one domain policy, in either input order. It accepts
separate field lines, a semicolon, or a comma as the separator. Repeated
domain policies and repeated response-local directives fail typed lookup.
Rendering puts the domain policy first and uses Adobe's semicolon syntax.
The original five names are available as bundled patterns and retain their
previous `Show` output.
[Adobe cross-domain configuration](https://www.adobe.com/devnet-docs/acrobatetk/tools/AppSec/xdomain.html).

X-XSS-Protection decodes the singleton authored disabled value `0`
and the enabled value `1` with optional `mode=block` and `report` directives.
It follows Chromium's case folding and rejection of repeated directives.
The reporting endpoint uses a nonempty RFC 3986 URI-reference, supports
relative references, and retains the accepted URI spelling.
[Chromium's historical HTTP parser](https://github.com/chromium/chromium/blob/3b8be2124e3f1d3ce09d534315fb9a8f50ead086/third_party/WebKit/Source/platform/network/HTTPParsers.cpp), [RFC 3986 URI-reference grammar](https://www.rfc-editor.org/rfc/rfc3986.html#section-4.1).

The public lookup and rendering paths agreed with an independent grammar on
182,045 vectors covering byte mutations, repeated lines, whitespace,
directive conflicts, and URI-reference syntax. All 926 Hermes tests and
65 focused HTTP client tests passed.

Sequential Criterion runs used GHC 9.10.3 on an Apple M5 Max. The Go
1.26.10 comparator uses typed byte or string checks and `net/url.Parse`
for the reporting endpoint.

| Parse fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| DNS prefetch | 31.6 ns / 456 B | 5.4 ns / 32 B | 4.61–4.66 ns / 0 B |
| Mixed-case DNS prefetch | | 5.1 ns / 32 B | 5.13–5.48 ns / 0 B |
| Download options | 33.7 ns / 472 B | 4.9 ns / 32 B | 2.66–2.71 ns / 0 B |
| Domain policy | 32.4 ns / 456 B | 7.4 ns / 48 B | 10.3–11.1 ns / 0 B |
| Domain policy and response-local directive across lines | | 13.5 ns / 96 B | 22.9–23.3 ns / 0 B |
| XSS block directive | 44.9 ns / 520 B | 6.3 ns / 32 B | 3.63–3.96 ns / 0 B |
| XSS block and report directives | 101 ns / 1,736 B | 123 ns / 664 B | 155–159 ns / 176 B |

Simplified Core shows bounded byte reads and shared successful results for
the bare DNS and download fixtures. The cross-domain codec parses at most
two original field lines without concatenating them. Reporting URI syntax
validation adds about 22 ns to the prior Hermes fixture, while eliminating
the intermediate directive list cuts allocation by about 62%.

### DPoP, replay nonces, and registration fields

The DPoP request field validates the RFC 9449 `token68` envelope and
preserves the proof bytes. Proof verification, including compact JWT and
claim validation, takes place at the application layer. DPoP-Nonce accepts a
nonempty sequence of OAuth `NQCHAR` octets, with commas retained as opaque
nonce data. Both reject repeated field lines.
[RFC 9449 §§4.1 and 4.3](https://www.rfc-editor.org/rfc/rfc9449.html#section-4.1), [RFC 9449 §8.1](https://www.rfc-editor.org/rfc/rfc9449.html#section-8.1), [RFC 6749 Appendix A](https://www.rfc-editor.org/rfc/rfc6749.html#appendix-A).

Replay-Nonce checks unpadded Base64url, including the final sextet's unused
bits, and retains the original token spelling. It applies HTTP field OWS
and rejects repeated lines.
[RFC 8555 §6.5.1](https://www.rfc-editor.org/rfc/rfc8555.html#section-6.5.1), [RFC 4648 §3.5](https://www.rfc-editor.org/rfc/rfc4648.html#section-3.5).

Hobareg decodes `regok` and `reginwork` case-insensitively and renders the
canonical lowercase forms. An invalid or repeated value has the prescribed
`reginwork` fallback. The existing Haskell constructors are retained.
The caller uses the response status to interpret `regok`.
[RFC 7486 §6.1](https://www.rfc-editor.org/rfc/rfc7486.html#section-6.1).

Optional-WWW-Authenticate uses the shared challenge grammar. Typed lookup
combines field lines in order, preserves quoted commas, and rejects a
present list with no challenges. Rendering an empty Haskell list emits
zero field lines.
[RFC 8053 §3](https://www.rfc-editor.org/rfc/rfc8053.html#section-3).

The tests exercise all 256 interior octets in DPoP and DPoP-Nonce through
typed lookup. An independent grammar and Python's Base64 codec agreed with
typed lookup and rendering on 386,981 cases across both DPoP fields,
Replay-Nonce, and Hobareg. The corpus includes every two- and three-character
Base64url value and checks ordered field lines, whitespace, and case.
All 931 Hermes tests and 65 focused HTTP client tests passed.

Sequential Criterion runs used GHC 9.10.3 on an Apple M5 Max. The Go
1.26.10 comparison uses byte-wise DPoP and nonce validators and the standard
library's strict Base64url decoder. Both implementations retain an owned
copy of the original spelling.

| Parse fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| 358-byte DPoP | 45 ns / 888 B | 89 ns / 688 B | 166–182 ns / 384 B |
| DPoP-Nonce | 37 ns / 552 B | 33 ns / 352 B | 19–21 ns / 24 B |
| Replay-Nonce | 12,700 ns / 208,208 B | 31 ns / 352 B | 31–33 ns / 24 B |

The previous DPoP codec simply captured the field body. The first
validating version took 229 ns on the same fixture. The token68 scan
now checks alphanumeric runs eight bytes at a time and falls back to the
full alphabet at punctuation. Simplified Core shows bounded loads and no
heap allocation in the scan loops. The DPoP-Nonce loop is scalar and the
remaining allocations occur around parsing and copying the compact result.

### Web Push fields

TTL is available in requests and responses. It accepts the full unsigned
`Word` range and uses `2147483648` when the decimal value exceeds that range.
The parser consumes the rest of the decimal run and still rejects a
non-whitespace suffix. Rendering writes ordinary decimal without leading zeros.
[RFC 8030 §5.2](https://www.rfc-editor.org/rfc/rfc8030.html#section-5.2).

Topic requires 1–32 characters from the Base64url alphabet. It preserves
the case and exact spelling of the opaque topic identifier, including
values whose lengths or final characters would be invalid in encoded data.
Topic and Replay-Nonce share the alphabet definition. Replay-Nonce additionally
checks its encoding length and last sextet.
[RFC 8030 §5.4](https://www.rfc-editor.org/rfc/rfc8030.html#section-5.4).

Urgency accepts ASCII case variants and renders the four lowercase tokens.
An absent value remains absent in typed lookup. The application can then
apply the normal push-message default or an unrestricted monitoring request,
depending on the message role.
[RFC 8030 §5.3](https://www.rfc-editor.org/rfc/rfc8030.html#section-5.3), [RFC 5234 §2.3](https://www.rfc-editor.org/rfc/rfc5234.html#section-2.3).

Independent grammars agreed with typed lookup and rendering on 137,288
Web Push cases. The corpus includes every byte inserted at each position
in representative values, values around the TTL overflow boundary,
and every case variant of the four Urgency tokens. The reused Base64url
alphabet also passed the earlier 386,981-case OAuth and ACME corpus.
All 934 Hermes tests and 65 focused HTTP client tests passed.

Sequential Criterion runs used GHC 9.10.3 on an Apple M5 Max. The Go
1.26.10 comparison uses `strconv` for TTL, byte-wise Topic validation
followed by a copy, and keyword switches for Urgency.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| Parse TTL | 36.5 ns / 536 B | 18.9 ns / 272 B | 10.8–10.9 ns / 0 B |
| Render TTL | 13.3 ns / 264 B | 14.2 ns / 264 B | 9.7–10.1 ns / 5 B |
| Parse Topic | 6,880 ns / 122,136 B | 27.8 ns / 360 B | 20.4–23.8 ns / 32 B |
| Parse Urgency | 34.5 ns / 456 B | 4.3 ns / 32 B | 3.7–4.0 ns / 0 B |
| Render Urgency | 13.1 ns / 264 B | 4.4 ns / 32 B | 1.7–1.8 ns / 0 B |
| Return the Urgency literal directly | | 4.1 ns / 32 B | |

Simplified Core shows an unboxed decimal accumulator for TTL and bounded
eight-byte reads for Topic. The Topic and Replay-Nonce scanner now handles
hyphens and underscores in the wide path. Urgency rendering returns a
shared literal. The same Criterion harness charges 4.1 ns and 32 B when
returning that already-rendered literal directly.

### Network Error Logging and Reporting-Endpoints

NEL combines physical field lines as JSON Field Values. The full combined
list must be syntactically valid JSON. Following the field definition in
Network Error Logging §4.1, typed lookup selects the first usable policy in
wire order. A zero lifetime removes the policy. Registration requires a
positive integer lifetime and a string group name. Sampling fractions use
exact decimal values in the inclusive unit interval, and the two header
lists accept strings. Missing optional fields take their specified defaults.
[Network Error Logging §§4.1–4.2](https://www.w3.org/TR/network-error-logging/#nel-response-header), [JSON Field Values §4](https://datatracker.ietf.org/doc/html/draft-reschke-http-jfv-08#section-4).

Decoded values retain the combined JSON spelling, including extension
members. The original raw constructor and record updates still work.
Code that coerced the old newtype to `ShortText` can use `nelPolicy`.
The checked authoring API emits one ASCII JSON object, with Unicode
escaped and default settings omitted. Integer lifetimes and sample fractions
use `Scientific` to avoid expanding large exponents. The lexical check
rejects raw controls in strings and exponents that exceed the decoder's
integer range.

Reporting-Endpoints still combines Structured Fields dictionary lines,
applies key replacement, and ignores invalid endpoint entries. The URI
validator now checks the complete reference without copying the string
again. Rendering writes dictionary separators directly into the builder.
The client resolves relative endpoints and applies its trust policy.
[Reporting API §§3.2–3.3](https://www.w3.org/TR/reporting-1/#header).

An independent Python JSON decoder and semantic projection agreed with
Hermes typed lookup and rendering on 210,795 NEL cases. The corpus covers
all octet mutations of representative inputs, ordered lines, Unicode and
escaped strings, sampling fractions, and large exponents. The existing
80,292-case signature and reporting matrix also passed. All 938 Hermes
tests and 65 focused HTTP client tests passed.

Sequential Criterion runs used GHC 9.10.3 on an Apple M5 Max. The Go
1.26.10 NEL comparator uses `encoding/json` with policy checks, and the
endpoint comparator uses `httpsfv` v1.1.2 and `net/url`.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| Parse and project two Reporting-Endpoints | 366 ns / 2,264 B | 337 ns / 1,752 B | 645–649 ns / 1,056 B |
| Render two Reporting-Endpoints | 147 ns / 1,264 B | 93 ns / 648 B | 222–225 ns / 280 B |
| Read a NEL registration | 38 ns / 600 B | 467 ns / 3,896 B | 557–586 ns / 480 B |
| Author and render a NEL registration | | 131 ns / 1,304 B | 79–85 ns / 80 B |

The previous NEL reader captured text without decoding JSON. The current
reader uses Aeson to materialize the policy object and retains the original
spelling. The initial typed authoring path used Aeson's general object
encoder and took 324 ns with 7,128 B of allocation. The final authoring
path writes directly into the HTTP builder and keeps the resulting bytes
for rendering. The JSON string encoder handles fields that need escaping.

### Delta-encoding fields

A-IM and IM combine physical lines before parsing the ordered list. Empty
list members are skipped. Each manipulation can carry one extension
parameter, which can be a bare flag or have a token or quoted value.
The typed representation keeps opaque quoted octets and canonicalizes
quoted pairs when rendering.
[RFC 3229 §10.1](https://www.rfc-editor.org/rfc/rfc3229.html#section-10.1), [RFC 3229 §10.5](https://www.rfc-editor.org/rfc/rfc3229.html#section-10.5), [RFC 9110 §5.2](https://www.rfc-editor.org/rfc/rfc9110.html#section-5.2).

A-IM allows a separate final quality weight with a case-insensitive `q`.
It accepts the original grammar's whitespace around separators and uses
the shared three-digit quality codec. IM accepts applied manipulations
and reserves `identity` and `q` for A-IM. Existing bare constructors and
record syntax still work. The names-only IM accessor projects away
parameters, with `instanceManipulationEntries` retaining the full list.
The change replaces the old coercion between IM and a list of names.
[RFC 2616 §2.1](https://www.rfc-editor.org/rfc/rfc2616.html#section-2.1), [RFC 9110 §12.4.2](https://www.rfc-editor.org/rfc/rfc9110.html#section-12.4.2).

Delta-Base retains the shared singleton entity-tag codec. The regression
tests check opaque weak tags and rejection of multiple physical lines.
The relationship between these fields, conditional request validators,
and status 226 belongs to message processing.
[RFC 3229 §§10.4–10.5](https://www.rfc-editor.org/rfc/rfc3229.html#section-10.4).

Typed lookup and canonical rendering matched an independent Go parser on
167,117 wire cases across A-IM, IM, and Delta-Base. The corpus covers all
octet mutations of representative values, quality lexemes, field order,
quoted pairs, and opaque entity tags. All 942 Hermes tests and 65 focused
HTTP client tests passed.

Sequential Criterion runs used GHC 9.10.3 on an Apple M5 Max. The Go
1.26.10 comparator parses the same list and parameter grammar into an owned
typed representation.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| Parse A-IM | 94 ns / 880 B | 64 ns / 624 B | 127–138 ns / 272 B |
| Parse IM | 96 ns / 832 B | 71 ns / 656 B | 115–118 ns / 272 B |
| Render A-IM | 333 ns / 3,160 B | 47 ns / 464 B | 46–49 ns / 56 B |
| Render IM | 121 ns / 1,256 B | 29 ns / 280 B | 40–41 ns / 56 B |
| Parse A-IM extension across lines | | 141 ns / 1,080 B | 229–244 ns / 336 B |
| Parse IM extensions across lines | | 128 ns / 1,192 B | 213–220 ns / 336 B |
| Render A-IM extension | | 75 ns / 600 B | 72–74 ns / 120 B |
| Render IM extensions | | 53 ns / 416 B | 67–70 ns / 120 B |

Simplified Core shows direct ASCII loads and unboxed cursor advancement on
the token scan. The renderers write separators into the builder and A-IM
uses the fixed-width quality formatter.

### Content Security Policy fields

Both CSP fields retain each policy separately across commas and repeated
field lines. Each policy keeps every directive name and value token in
wire order. The per-policy projection folds directive names to lowercase
and keeps the first occurrence of each name. A name repeated in a different
policy is independent. The renderer joins policies with commas and
directives with semicolons.
[CSP3 §§2.2.1–2.2.2](https://www.w3.org/TR/CSP3/#parse-serialized-policy), [CSP3 §3](https://www.w3.org/TR/CSP3/#policy-delivery).

The HTTP codecs apply the ASCII name and value grammar with SP and HTAB
field whitespace. The standalone policy parser recognizes the wider Infra
ASCII whitespace used by HTML. The header parser skips empty fragments
and retains an explicitly empty field as an empty policy. It rejects
malformed serialized directives in any field line.
[CSP3 §§2.1–2.3](https://www.w3.org/TR/CSP3/#framework), [RFC 9110 §5.5](https://www.rfc-editor.org/rfc/rfc9110.html#section-5.5).

The single-policy constructors and record syntax still work. The legacy
directive selectors read the first policy. Use `cspPolicies` or
`csproPolicies` for the full field. Code that coerced an old newtype to a
directive list can use the corresponding legacy selector.

Typed lookup, canonical rendering, and the first-occurrence projection
matched an independent Go implementation on 361,686 cases across both
field names. The corpus covers all octet mutations of representative
inputs, policy boundaries, empty fragments, extension names, and ASCII
case variants. All 945 Hermes tests and 65 focused HTTP client tests passed.

Sequential Criterion runs used GHC 9.10.3 on an Apple M5 Max. The Go
1.26.10 comparator performs the same validation and retains owned strings.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| Parse one policy | 56,460 ns / 867,704 B | 192 ns / 1,424 B | 397–441 ns / 544 B |
| Render one policy | 466 ns / 4,448 B | 89 ns / 408 B | 81–85 ns / 240 B |
| Parse three policies across two lines | | 291 ns / 2,216 B | 611–673 ns / 920 B |
| Render three policies | | 160 ns / 976 B | 119–121 ns / 496 B |
| Project the first directive per name across three policies | | 279 ns / 2,672 B | 252–271 ns / 320 B |

Simplified Core shows direct byte loops with unboxed cursors for names
and values. The scanner copies each completed token into compact storage.
The renderer writes separators directly into the builder.

### Last-Event-ID and Refresh

Last-Event-ID validates UTF-8 and excludes NUL, LF, and CR. A comma,
whitespace, and an explicitly empty value are preserved as identifier
data. Typed lookup rejects repeated field lines.
[HTML §9.2.4](https://html.spec.whatwg.org/multipage/server-sent-events.html#the-last-event-id-header).

Refresh supports comma, semicolon, or whitespace separators, an optional
ASCII case-insensitive URL prefix, and quoted or unquoted targets. The
codec projects fractional delays to the whole-second part and rejects
counts beyond the range of Word. It preserves isomorphically decoded
target octets, applies HTTP field byte rules, and distinguishes an empty
target from a bare delay. HTML leaves repeated field lines unspecified.
Hermes keeps its existing first-line policy. Resolving the extracted
target and applying navigation policy require a document context.
[HTML refresh steps](https://html.spec.whatwg.org/multipage/semantics.html#shared-declarative-refresh-steps),
[HTML navigation](https://html.spec.whatwg.org/multipage/document-lifecycle.html#loading-a-document),
[RFC 9110 §5.5](https://www.rfc-editor.org/rfc/rfc9110.html#section-5.5).

Typed lookup and canonical rendering matched an independent Go codec on
284,820 cases, including all byte insertions and substitutions in
representative values, repeated lines, and generated input. All 952
Hermes tests and 65 focused HTTP client tests passed.

Sequential Criterion runs used GHC 9.10.3 on an Apple M5 Max. The Go
1.26.10 comparator retains owned output bytes.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| Parse ASCII event ID | 38.6 ns / 552 B | 17.3 ns / 88 B | 22.5–23.1 ns / 24 B |
| Parse UTF-8 event ID | 37.1 ns / 544 B | 18.0 ns / 96 B | 19.7–20.2 ns / 16 B |
| Render ASCII event ID | | 12.4 ns / 144 B | 8.8–8.9 ns / 24 B |
| Parse refresh delay | 39.1 ns / 528 B | 8.5 ns / 144 B | 8.5–9.0 ns / 0 B |
| Parse refresh with target | 46.2 ns / 624 B | 47.1 ns / 248 B | 38.8–42.5 ns / 24 B |
| Render refresh with target | 20.6 ns / 280 B | 30.8 ns / 312 B | 16.5–16.7 ns / 32 B |

Simplified Core shows unboxed cursors in byte scans and direct conversion
from compact text to strict bytes for Last-Event-ID rendering. Refresh
rendering scans for non-ASCII bytes and handles leading syntax characters
before copying the target. The non-ASCII conversion is out of line.
The generic builder still allocates its initial 128-byte pinned buffer
and state, which the Go comparator supplies on the stack.

### ALPN and HTTP2-Settings

The RFC 7639 ALPN list combines all field lines and skips empty members.
At least one protocol identifier must remain. The decoder accepts opaque
protocol octets, requires uppercase hex escapes for non-token octets and
`%`, and rejects escaped token octets with a literal spelling. The
identifier limit applies to the 255 decoded octets.
[RFC 7639 §§2.2–2.3](https://www.rfc-editor.org/rfc/rfc7639.html#section-2.2),
[RFC 7301 §3.1](https://www.rfc-editor.org/rfc/rfc7301.html#section-3.1).

The legacy HTTP2-Settings field uses one nonempty, unpadded Base64url
token containing complete six-octet settings entries. This follows the
published token68 grammar, including its nonempty requirement. The
compatibility constructor and raw payload accessor remain available.
`http2SettingsEntries` and `http2SettingsFromEntries` expose the ordered
identifier and value records. Setting-specific validation and application
remain with the HTTP/2 connection.
[RFC 7540 §3.2.1](https://www.rfc-editor.org/rfc/rfc7540.html#section-3.2.1),
[RFC 7540 §6.5](https://www.rfc-editor.org/rfc/rfc7540.html#section-6.5),
[RFC 9113 §11.1](https://www.rfc-editor.org/rfc/rfc9113.html#section-11.1).

Typed lookup and canonical rendering matched an independent Go codec on
173,962 cases spanning repeated field lines, all octet insertions and
substitutions in representative fields, identifier size boundaries, and
generated settings values. All 958 Hermes tests and 65 focused HTTP
client tests passed.

Sequential Criterion runs used GHC 9.10.3 on an Apple M5 Max and the
comparison used Go 1.26.10.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| Parse bare ALPN | 61.5 ns / 704 B | 10.6 ns / 88 B | 26–27 ns / 16 B |
| Parse ALPN list with escaped ID | 137 ns / 1,328 B | 66.8 ns / 696 B | 64.5–65.8 ns / 40 B |
| Render ALPN list | 106 ns / 968 B | 51.4 ns / 408 B | 54.3–55.7 ns / 16 B |
| Parse HTTP2-Settings | 4,789 ns / 78,688 B | 30.5 ns / 344 B | 30.7–32.1 ns / 16 B |
| Render HTTP2-Settings | 61.7 ns / 696 B | 23.6 ns / 392 B | 15.5–15.6 ns / 16 B |

Simplified Core shows raw offset loops for field scans and fixed byte
loads for the typed settings view. The ALPN renderer copies runs of token
octets directly. Removing the generic parser and builder around
HTTP2-Settings reduced that path to 82 ns to parse and 46 ns to render
with the original memory dependency. Using the existing
`base64-bytestring` dependency from the wider Wireform stack avoided that
generic byte-array conversion. The remaining render gap is in the
library's strict ByteString allocation and encoder, compared with Go's
Base64url encoder returning a short string.

### Content-MD5, MIME-Version, and Slug

Content-MD5 accepts one canonical padded Base64 digest of exactly
16 octets. The parser and WebSocket handshake fields share validation of
decoded width and padding bits. `mkContentMD5` checks the digest width
when constructing a field from bytes.
[RFC 1864 §2](https://www.rfc-editor.org/rfc/rfc1864.html#section-2).

MIME-Version accepts one field line, skips comments between tokens,
and validates both numeric components through the maximum `Int` value.
Nested comments and quoted pairs use the HTTP comment scanner. Rendering
emits the decimal pair without comments or padding.
[RFC 2045 §4](https://www.rfc-editor.org/rfc/rfc2045.html#section-4),
[RFC 9112 Appendix B.1](https://www.rfc-editor.org/rfc/rfc9112.html#appendix-B.1).

Slug accepts one field line with a valid percent-encoded UTF-8 title.
The codec preserves its ASCII wire spelling, including the case of
escapes. `slugDecodedText` provides the Unicode title.
`slugFromText` uses uppercase escapes and encodes surrounding spaces
so an HTTP message parser can remove field whitespace without changing
the title.
[RFC 5023 §9.7.1](https://www.rfc-editor.org/rfc/rfc5023.html#section-9.7.1).

Typed lookup and canonical rendering matched an independent Go codec
on 245,654 wire cases. A separate Python calculation matched 20,260
authored Slug values across ASCII, Unicode, and surrounding whitespace.
All 965 Hermes tests and 65 focused HTTP client tests passed.

Sequential Criterion runs used GHC 9.10.3 on an Apple M5 Max and the
comparison used Go 1.26.10.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| Parse Content-MD5 | 12,220 ns / 212,344 B | 24.8 ns / 320 B | 34.0–35.9 ns / 24 B |
| Render Content-MD5 | 64.8 ns / 744 B | 15.9 ns / 200 B | 17.1–17.9 ns / 24 B |
| Parse MIME-Version 1.0 | 40.2 ns / 560 B | 5.0 ns / 48 B | 0.94–0.95 ns / 0 B |
| Parse MIME-Version 123.456 | 43.4 ns / 624 B | 32.6 ns / 328 B | 26.0–26.8 ns / 0 B |
| Parse MIME-Version with nested comments | | 50.3 ns / 328 B | 45.5–46.2 ns / 0 B |
| Render MIME-Version 1.0 | 12.5 ns / 280 B | 4.3 ns / 32 B | 0.94–0.95 ns / 0 B |
| Render MIME-Version 123.456 | | 14.6 ns / 280 B | 17.3–18.0 ns / 8 B |
| Parse ASCII Slug | 36.1 ns / 552 B | 24.7 ns / 88 B | 18.4–19.5 ns / 24 B |
| Parse escaped Slug | 37.0 ns / 552 B | 58.8 ns / 328 B | 76.9–78.5 ns / 48 B |
| Render escaped Slug | 12.9 ns / 264 B | 12.6 ns / 144 B | 8.8–9.1 ns / 24 B |
| Decode a Slug title | | 64.3 ns / 584 B | 55.5–56.5 ns / 24 B |
| Construct Slug from a Unicode title | 240 ns / 2,648 B with a general URL builder | 69.1 ns / 648 B | 49.9–51.0 ns / 24 B |

Core shows the Content-MD5 path calling the fixed-width Base64 worker
directly and the Slug encoder writing escaped bytes with unboxed
offsets. The MIME-Version 1.0 path returns a static typed result or a
static ByteString. It remains several nanoseconds behind Go's raw
literal path in the two harnesses. Matching the literal with three
bounded byte loads removed the general ByteString comparison call.

### Surrogate-Capability and Surrogate-Control

Surrogate-Capability combines field lines in advertised order and
rejects a device identifier repeated anywhere in the field. It requires
a quoted, nonempty list of capability products. Product names and
versions retain their case. `surrogateCapabilityFeatures` provides the
ordered name/version records and `surrogateCapabilityFromFeatures`
builds an entry from them.
[Edge Architecture §2.1](https://www.w3.org/TR/edge-arch/).

Surrogate-Control combines ordered field lines and retains each
directive's device targets. The typed view exposes the two storage
flags, max-age with its freshness extension, ordered content
capabilities, and Fastly's two stale-serving delays. Delay projections
saturate at the largest representable Word. The raw directive name
and argument accessors remain available, and quoted extension bytes
round-trip after canonical quoting. `surrogateControlFromKind` builds
directives from the typed arguments and a target list.
[Edge Architecture §§2.2–2.3](https://www.w3.org/TR/edge-arch/),
[Fastly stale-serving directives](https://www.fastly.com/documentation/guides/concepts/cache/stale/).

Typed lookup and canonical rendering matched an independent Go codec on
242,342 wire cases spanning repeated lines, targeted directives, opaque
quoted extension bytes, and all octet insertions and substitutions in
representative fields. All 970 Hermes tests and 65 focused HTTP client
tests passed.

Sequential Criterion runs used GHC 9.10.3 on an Apple M5 Max. The Go
comparison used 1.26.10 and retained owned output values.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| Parse two capability entries | 606 ns / 6,976 B | 85.6 ns / 688 B | 197–201 ns / 141 B |
| Parse capabilities across two lines | | 103 ns / 1,016 B | 199–203 ns / 141 B |
| Render two capability entries | 756 ns / 7,096 B | 32.1 ns / 144 B | 29.2–29.6 ns / 64 B |
| Parse four control directives | 379 ns / 4,096 B | 170 ns / 1,136 B | 367–373 ns / 512 B |
| Parse targeted directives across two lines | | 207 ns / 1,736 B | 501–509 ns / 608 B |
| Render four control directives | 501 ns / 4,648 B | 80.6 ns / 344 B | 70.4–71.8 ns / 80 B |

Core shows unboxed byte cursors in the list and product scanners.
Capability rendering uses a single exactly sized output buffer.
Control rendering copies compact ASCII arguments directly and delegates
escaped or opaque arguments to the shared HTTP quoted-string renderer.

### SOAPAction and Meter

SOAPAction distinguishes a missing field, a present empty field, a
quoted empty URI, and a quoted explicit reference. `soapActionIntent`
exposes the three present states. The old `SoapAction` constructor and
`soapActionURI` view remain available. The legacy view reads both empty
forms as empty text. SOAP 1.1 cites RFC 2396. Hermes uses the shared
RFC 3986 successor grammar for quoted references and retains the exact
accepted spelling. Repeated lines are rejected.
[SOAP 1.1 §6.1.1](https://www.w3.org/TR/2000/NOTE-SOAP-20000508/#_Toc478383528),
[RFC 3986 §4.1](https://www.rfc-editor.org/rfc/rfc3986.html#section-4.1).

Meter combines physical field lines in order and supports mixed long
and abbreviated directives. It parses the required unquoted use/reuse
pair for `count`, validates the numeric limits and the timeout, and
checks direction when supplied in HeaderSettings. The typed view uses
arbitrary-precision natural numbers and expresses the timeout in minutes.
Typed authoring emits the RFC abbreviations. A present empty list stays
distinct from a missing field so the caller can apply the appropriate
request or response default. Unknown extensions retain their accepted
value bytes for forwarding.
[RFC 2227 §§5.1–5.2](https://www.rfc-editor.org/rfc/rfc2227.html#section-5).

Typed lookup and rendering matched an independent Python grammar on
182,989 cases across both fields. The corpus includes direction-aware
Meter lookups, quoted extension bytes, URI authority and escape
boundaries, and all byte insertions and substitutions in representative
inputs. All 976 Hermes tests and 65 focused HTTP client tests passed.

Sequential Criterion runs used GHC 9.10.3 on an Apple M5 Max. Go
1.26.10 supplied the compiled comparison. Its SOAP parser uses
`net/url.Parse` and retains the accepted URI spelling.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| Parse SOAPAction with absolute URI | 519 ns / 6,711 B | 107 ns / 400 B | 198–207 ns / 192 B |
| Parse SOAPAction with relative URI | | 50.2 ns / 384 B | 174–179 ns / 200 B |
| Render quoted SOAPAction | 213 ns / 3,840 B | 13.6 ns / 144 B | 18.3–18.8 ns / 48 B |
| Parse three abbreviated Meter directives | 84.2 ns / 968 B | 79.4 ns / 912 B | 166–176 ns / 293 B |
| Parse Meter count and flags across two lines | | 123 ns / 1,256 B | 207–221 ns / 328 B |
| Render three abbreviated Meter directives | 136 ns / 1,480 B | 36.2 ns / 312 B | 32.3–33.0 ns / 16 B |

Core shows the shared URI scanner advancing raw cursors and the SOAP
renderer copying the reference into an exactly sized buffer. Meter
dispatches abbreviated names from one input byte and copies ordinary
rendered values in blocks.

### Negotiate, TCN, and Variant-Vary

All three fields combine physical lines in order and skip empty list
positions. Negotiate and Variant-Vary require a member after
combination. TCN preserves a present empty list.
[RFC 2295 §§8.4–8.6](https://www.rfc-editor.org/rfc/rfc2295.html#section-8.4).

The Negotiate typed view identifies the bare keywords and RVSA versions
with one through four digits per component. It retains other valid
tokens and parameterized entries as extensions. The raw list preserves
capitalization and redundant version digits. Typed authoring emits
normalized version numbers.
[RFC 2295 §7.1](https://www.rfc-editor.org/rfc/rfc2295.html#section-7.1).

TCN keeps response types, server overrides, and extension directives
in order. Its typed view treats entries with arguments as extensions.
It validates quoted extensions and preserves their decoded octets,
including obs-text. Variant-Vary rejects a wildcard combined with
other members. Its typed accessor returns case-folded HTTP field names
while the renderer preserves the original spelling.

Typed lookup, wire rendering, and normalized typed projections matched
an independent Go codec across 219,060 cases. The corpus covers
repeated lines, version boundaries, extension octets, and all octet
insertions and substitutions in representative fields. All 982 Hermes
tests and 65 focused HTTP client tests passed.

Sequential Criterion runs used GHC 9.10.3 on an Apple M5 Max. The
compiled comparison used Go 1.26.10 and retained owned output values.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| Parse Negotiate | 102 ns / 1,120 B | 85.4 ns / 800 B | 239–242 ns / 312 B |
| Render Negotiate | 163 ns / 1,688 B | 36.6 ns / 280 B | 36.2–36.9 ns / 48 B |
| Parse TCN | 292 ns / 3,280 B | 93.2 ns / 736 B | 222–240 ns / 320 B |
| Render TCN | 212 ns / 2,336 B | 56.7 ns / 280 B | 57.4–59.1 ns / 48 B |
| Parse Variant-Vary | 84.1 ns / 776 B | 86.4 ns / 616 B | 119–125 ns / 112 B |
| Render Variant-Vary | 118 ns / 1,280 B | 28.3 ns / 280 B | 25.2–25.5 ns / 48 B |

Core shows unboxed cursors in the list scanners and direct block copies
from compact text in all three renderers. The shared quoted-string
renderer handles the escaped and opaque TCN extension cases.


### Accept-Features and Alternates

Accept-Features exposes the wildcard, tag presence and absence, equality,
inequality, and exclusive values. Tags compare case-insensitively and
feature values compare as octets after quoted-pair and percent decoding.
Feature extensions retain their decoded bytes. An empty field remains
distinct from an absent field.
[RFC 2295 §§6.1, 8.2](https://www.rfc-editor.org/rfc/rfc2295.html#section-6.1).

Alternates exposes ordered variants, fallback references, proxy algorithm
versions and extension directives. Variants have typed source quality,
media type, charset, languages, length, and description. Their feature lists
support predicate bags, open-ended integer ranges and decimal quality factors
represented as exact thousandths. Duplicate attributes and multiple fallback
references fail validation. Repeated descriptions of a variant remain
ordered. Unknown attribute bodies retain exact bytes.
[RFC 2295 §§5, 6.3–6.4, 8.3](https://www.rfc-editor.org/rfc/rfc2295.html#section-5).

Typed lookup and authoring run through the public HeaderMap API. The
legacy raw patterns still construct and project text. Rendering a parsed
field reuses the validated bytes, with comma joining for repeated field
lines. Tests include every quoted-pair octet in both strict ASCII feature
values and opaque extensions. A fixture captured from Apache 2.4.67
mod_negotiation verified the typed list, including Apache's encoding
extension, and byte-exact rendering. All 991 Hermes tests and 65 focused
HTTP client tests passed.
[Apache mod_negotiation](https://github.com/apache/httpd/blob/trunk/modules/mappers/mod_negotiation.c).

Sequential Criterion measurements used GHC 9.10.3. The previous parse
fixtures copied the field text. The current parse fixtures validate the
grammar and build typed values. The rendering fixtures use a pre-parsed
value. Allocations are bytes per operation.

| Fixture | Previous Hermes | Current Hermes |
| --- | ---: | ---: |
| Parse Accept-Features and project raw text | 40.7 ns / 584 B | 234 ns / 1,680 B |
| Parse and project typed Accept-Features | | 220 ns / 1,592 B |
| Render parsed Accept-Features | 16.4 ns / 264 B | 5.2 ns / 32 B |
| Parse Alternates and project raw text | 42.0 ns / 680 B | 666 ns / 3,816 B |
| Parse and project typed Alternates | | 561 ns / 3,632 B |
| Render parsed Alternates | 38.0 ns / 895 B | 5.3 ns / 32 B |

Core shows unboxed input cursors in the token and URI scanners, with typed
allocation at member boundaries. Parser boundaries reduced the two modules'
combined optimized object size from 1.6 MiB to approximately 1.0 MiB compared
with the initial implementation. Throughput against a full external typed
decoder remains unmeasured.


### OSCORE and Token Binding

OSCORE enforces canonical unpadded base64url and maps AA to an empty CoAP
option. The decoded CoAP bytes stay opaque so the HTTP codec also preserves
options defined by extensions. The checked wire form in RFC 8613 carries the
option in both request and response directions.
[RFC 8613 §§11.1–11.3](https://www.rfc-editor.org/rfc/rfc8613.html#section-11.1).

Sec-Token-Binding enforces the binary vector boundaries and the HTTP
requirements for one provided binding and at most one referred binding.
The typed entries retain public keys, signatures, extensions, and
unrecognized code points. Known key and signature formats are checked
structurally. Signature verification requires the TLS context.
[RFC 8471 §3](https://www.rfc-editor.org/rfc/rfc8471.html#section-3),
[RFC 8473 §2](https://www.rfc-editor.org/rfc/rfc8473.html#section-2).

Include-Referred-Token-Binding-ID accepts the case-insensitive true response
signal and writes lowercase. An authored false emits no field line.
[RFC 8473 §5.3](https://www.rfc-editor.org/rfc/rfc8473.html#section-5.3).

The RFC examples and the public HeaderMap suite exercise typed decoding,
authoring, singleton handling, and exact encoded rendering. Hermes and an
independent Go codec agreed on 308,110 cases. The corpus covers all
two-octet OSCORE values, small encodings, base64 tail-bit and alphabet
boundaries, binding frames and extensions, and all octet substitutions
in the RFC Token Binding example. The 992 Hermes tests and 65 focused HTTP
client tests passed.

Sequential Criterion runs used GHC 9.10.3 on Apple M5 Max. The Go 1.26.10
comparison returned owned payloads and typed entries. Previous parsing
figures measure the raw text copy or literal flag parser. Current parsing
figures measure validation and typed projection. Bytes are allocations
per operation.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| Parse OSCORE | 38.8 ns / 536 B | 31.7 ns / 464 B | 24.5–24.7 ns / 3 B |
| Parse empty OSCORE mapping | 37.4 ns / 536 B | 11.1 ns / 72 B | 6.76–6.88 ns / 0 B |
| Encode a ten-byte OSCORE text fixture | | 26.9 ns / 408 B | |
| Parse Sec-Token-Binding | 56.2 ns / 720 B | 232 ns / 1,520 B | 180–185 ns / 224 B |
| Author Sec-Token-Binding from entries | | 166 ns / 1,808 B | 127–248 ns / 336 B |
| Render parsed Sec-Token-Binding | 37.8 ns / 960 B | 4.78 ns / 32 B | 1.66–1.69 ns / 0 B |
| Parse Include-Referred-Token-Binding-ID | 32.4 ns / 456 B | 5.30 ns / 32 B | 2.72 ns / 0 B |

The Core dump shows unboxed length and cursor operations in the binary
parser. Authoring from typed binding entries validates before writing
and keeps the entries without decoding the resulting bytes again. The
OSCORE encoder writes short options into one pinned output. The
encoding follow-up below corrects the short-option benchmark fixture.
The codecs return the original wire bytes when rendering a parsed field.


### Cookie2 and Set-Cookie2

Cookie2 parses one case-insensitive version directive, accepts quoted
and bare decimal versions without a machine-word limit, and rejects
additional field lines. Set-Cookie2 decodes one or more cookie definitions
per line and preserves physical field-line boundaries. Each definition
requires a version. The typed view takes the first occurrence of each
case-insensitive attribute name while the renderer preserves the
original bytes. Quoted Version is accepted as shown in the RFC examples.
[RFC 2965 §§3.2.2, 3.3.5, and 4](https://www.rfc-editor.org/rfc/rfc2965.html#section-3.2.2),
[RFC 2965 author notes](https://kristol.org/cookie/errata.html#7).

Cookie values and extension arguments preserve opaque quoted octets.
Comment validates UTF-8. CommentURL requires a quoted absolute HTTP URL.
Port distinguishes a bare attribute, which means the original request
port, from a nonempty quoted list of 16-bit ports. Version and Max-Age
use arbitrary-precision non-negative integers. The codec leaves cookie
storage and request matching to the caller.
[RFC 2965 §§3.2.2 and 3.3](https://www.rfc-editor.org/rfc/rfc2965.html#section-3.3).

Public typed lookup, projections and exact rendering matched an independent
Go codec across 197,079 cases, including every port value, malformed and
opaque quoted octets, duplicate attributes, and folded and repeated field
lines. CPython's RFC 2965 CookieJar produced a Cookie2 field accepted by
Hermes and agreed on the decoded cookies in a folded and repeated
Set-Cookie2 response. All 993 Hermes tests and 65 focused HTTP client
tests passed.
[CPython http.cookiejar](https://docs.python.org/3/library/http.cookiejar.html).

Sequential Criterion runs used GHC 9.10.3 on Apple M5 Max. The previous
parser figures measure raw-text copies. The current parser figures measure
typed validation and projection. The Go 1.26.10 comparison retained the
parsed records. Render figures use pre-parsed headers.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| Parse Cookie2 and project raw text | 39.4 ns / 544 B | 37.9 ns / 464 B | |
| Parse and project Cookie2 version | | 34.1 ns / 432 B | 25.1–25.3 ns / 0 B |
| Render parsed Cookie2 | 14.8 ns / 264 B | 5.4 ns / 32 B | |
| Parse Set-Cookie2 and project raw text | 47.9 ns / 704 B | 279 ns / 2,048 B | |
| Parse and project one typed cookie | | 235 ns / 1,936 B | 382–394 ns / 840 B |
| Parse and project two folded cookies | | 306 ns / 2,384 B | 489–503 ns / 1,016 B |
| Render parsed Set-Cookie2 | 21.0 ns / 352 B | 10.6 ns / 120 B | |

Core shows an unboxed word comparison for the Cookie2 directive and
unboxed input cursors in the response parser. Common attribute spellings
reuse static keys, reducing allocation on the one-cookie fixture from
2,192 bytes in the first typed parser to 1,936 bytes.


### Early cross-site access and Security-Scheme

Method-Check reads a single case-sensitive method token and exposes the
shared Method type, including extension methods. Method-Check-Expires
uses all three HTTP-date grammars and honors the supplied reference clock
for the RFC 850 two-digit year. Parsed dates retain their wire spelling.
Authoring from a timestamp emits IMF-fixdate at whole-second precision.
[W3C Access Control for Cross-site Requests §4](https://www.w3.org/TR/2007/WD-access-control-20071126/).

Referer-Root distinguishes the literal null from a scheme and host with
a required decimal port. This includes explicit default ports and
bracketed IPv6 literals. It rejects a trailing host dot and extra URI
components. Checked authoring retains the typed components.
[W3C referrer root URI](https://www.w3.org/TR/2007/WD-access-control-20071126/).

Security-Scheme accepts only the S-HTTP/1.4 version, with ASCII case
folding and ordinary surrounding field whitespace. Authored values have
the spelling in the RFC. The field is available in both directions.
[RFC 2660 §4.1](https://www.rfc-editor.org/rfc/rfc2660.html#section-4.1).

The final suite passed 997 Hermes tests and 65 focused HTTP client tests.
Public typed lookup and rendering matched an independent Go implementation
on 160,009 cases, including all 65,536 port values, canonical dates from
1900 through 2099, every ASCII case of the S-HTTP literal, and additions
and substitutions at byte boundaries. The Hermes suite separately checks
RFC 850 and asctime dates and the reference-clock cutoff.

Sequential Criterion measurements used GHC 9.10.3 on Apple M5 Max. The
Go 1.26.10 comparison retained the parsed results and checked the date's
weekday. Previous Hermes parses copied raw text. Current typed parse
and render figures are measured separately.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| Parse Method-Check and project a typed method | | 8.7 ns / 48 B | 4.73–4.79 ns / 0 B |
| Parse Method-Check and project raw text | 42.6 ns / 536 B | 12.2 ns / 96 B | |
| Parse and project the expiry timestamp | | 112 ns / 1,024 B | 87.9–91.6 ns / 0 B |
| Render a parsed expiry | 17.2 ns / 264 B | 4.7 ns / 32 B | |
| Parse and project an IPv6 referer root | | 109 ns / 1,200 B | 43.0–44.9 ns / 16 B |
| Parse the null referer root | | 7.6 ns / 64 B | 4.92–5.27 ns / 0 B |
| Render a parsed referer root | 13.7 ns / 264 B | 5.0 ns / 32 B | |
| Parse Security-Scheme | 37.6 ns / 544 B | 7.9 ns / 48 B | 7.44–7.87 ns / 0 B |

Core shows that standard methods avoid the extension-token scan and that
the S-HTTP fallback compares its eight-byte prefix as an unboxed word.
The IPv6 referer-root fixture remains above the repository's twofold
throughput bar. The typed timestamp and origin retain the original
validated bytes for rendering.

## Proxy cache notification

Proxy-Features decodes the named proxy's notification and logging
capabilities in order. A log offer includes an absolute URI and one or
more expiry or peak-hours settings. Hermes accepts both spellings used
by the draft for logging and expiry, and renders authored values with
`logging` and `expires`. The draft's semicolon-delimited syntax shares
a delimiter with URIs. Hermes treats a semicolon as a URI boundary when
it introduces a recognized directive name with a field delimiter, and
keeps ordinary URI semicolons in the URI. Percent-encoded semicolons
remain URI data. The field rejects a log offer alongside active logging.
Proxy-Instruction accepts one of the draft's three mutually exclusive
commands. The existing singleton cardinality applies to each field.
[W3C Notification for Proxy Caches working draft](https://www.w3.org/TR/WD-proxy).

The public header-map and authoring tests cover the draft example,
ordered options, case and whitespace preservation, DNS label and name
length limits, absolute URIs, expiry larger than 64 bits, all 24 peak
bits, malformed inputs, and repeated field lines. The GHC 9.10.3 run
passed 1,002 Hermes tests and 65 focused HTTP client tests. An independent
Go parser matched typed results and retained wire bytes on 78,721 cases.
The corpus included insertions and substitutions at byte boundaries for
each command and the standalone capabilities, and generated log offers
with different URI shapes, masks and expiry hours.

Sequential Criterion measurements used GHC 9.10.3 on Apple M5 Max.
The local Go 1.26.10 implementation used the standard URI parser and
retained the parsed values. Previous Hermes parse timings measured
raw-text storage. Current typed parse timings force the decoded
command or the feature tree, including each log option.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| Parse and project raw notification capability | 38.1 ns / 552 B | 65.7 ns / 544 B | |
| Parse and project typed notification capability | | 62.8 ns / 520 B | 69.7–70.6 ns / 80 B |
| Parse and project raw log offer | 39.6 ns / 616 B | 313 ns / 1,592 B | |
| Parse and project typed log offer | | 318 ns / 1,520 B | 370–376 ns / 480 B |
| Parse and project raw instruction | 39.9 ns / 552 B | 48.6 ns / 480 B | |
| Parse and project typed instruction | | 44.3 ns / 440 B | 25.3–26.1 ns / 0 B |
| Render parsed features | 12.3 ns / 264 B | 4.6 ns / 32 B | |
| Render parsed instruction | 12.6 ns / 264 B | 4.7 ns / 32 B | |

Core shows the DNS checker using unboxed offsets and label widths.
The common lowercase log command matches directly without allocating
a command token. Combining name lexing and validation into one custom
parser allocated 160 additional bytes per operation, so the measured
implementation uses the shared token scanner followed by the DNS
checker. The typed instruction and allocation gaps against Go remain
open.

## HTTP Extension Framework

Man and Opt share the RFC 2774 extension declaration codec. It
distinguishes absolute URI and field-name identifiers, keeps namespace
prefix spelling and leading zeroes, and retains ordered parameters with
absent, token, or quoted arguments. The codec reserves NS for a single
namespace binding immediately after the identifier. Quoted arguments
expose decoded octets while the header map retains their wire escaping.
Field-name identifiers receive lexical validation. An extension's
registered meaning and any permission to reuse a namespace are
message-level concerns. Ext accepts empty or whitespace-only
acknowledgements. [RFC 2774 §§3–4](https://www.rfc-editor.org/rfc/rfc2774.html#section-3).

The public tests exercise the RFC example, repeated and empty list
lines, composed header-map lookup and rendering, large and malformed
namespace prefixes, quoted commas and opaque bytes, malformed URI
syntax, and the difference between omission and an empty Ext. The
GHC 9.10.3 run passed 1,004 Hermes tests and 65 focused HTTP client
tests. A Go implementation agreed on typed values and bytes emitted
by the header map for 36,173 wire cases. The corpus includes generated
declarations, byte-boundary mutations of identifiers and parameters,
and malformed acknowledgement values.

Sequential Criterion measurements used GHC 9.10.3 on Apple M5 Max.
Previous parses measured raw text. Typed measurements force identifiers,
namespace prefixes, and parameters. Go 1.26.10 parses the same fixtures
and retains the parsed values.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| Parse and project raw Man URI declaration | 37.9 ns / 600 B | 206 ns / 1,408 B | |
| Parse and project typed Man URI declaration | | 186 ns / 1,320 B | 306–313 ns / 496 B |
| Parse and project raw Opt field-name declaration | 37.7 ns / 536 B | 24.2 ns / 232 B | |
| Parse and project typed Opt field-name declaration | | 20.9 ns / 208 B | 33–36 ns / 64 B |
| Parse empty Ext | 37.0 ns / 528 B | 8.0 ns / 48 B | |
| Render parsed Man using its builder | 12.1 ns / 264 B | 14.8 ns / 312 B | |
| Render parsed Man through the typed field renderer | | 10.8 ns / 128 B | |

Core shows the quoted field-name fast path checking the input slice with
unboxed offsets and constructing the declaration after validation.
The typed header renderer passes the received bytes through to the header
map. The declaration parse fixtures allocate more than the Go reference,
and standalone builder rendering remains slower than its raw predecessor.
Moving field-line retention into the shared list codec brought the
typed Man fixture to 167 ns and 1,200 B per operation. The typed Opt
fixture took 18.8 ns and 208 B.

## PEP declarations and policies

The November 1997 PEP draft defines separate grammars for applied
extensions and extension policies. PEP decodes a required map,
relative or absolute URI references, decimal header prefixes, and
strength. PEP-Info decodes a required policy identifier, URI targets
with an optional whitespace-delimited wildcard, an explicitly empty
target list, arbitrary-precision delta seconds, and nested parameters.
Both fields retain unknown bags in order and preserve quoted bytes,
accepted field whitespace, and physical field lines. Checked authoring
renders the typed value and validates the complete wire grammar.
[W3C PEP draft, sections 3–5](https://www.w3.org/TR/WD-http-pep-971121).

Tests cover draft examples, generated quoted octets and lifetimes,
repeated fields, incomplete or malformed bags, and 128 levels of
nested extension parameters. The GHC 9.10.3 suite passed 1,006 Hermes
tests and 65 focused HTTP client tests. A separate Go bag parser and
validator agreed on 44,289 cases over canonical rendering from the
typed trees and the original bytes emitted by the header map.

Sequential Criterion measurements used GHC 9.10.3 on Apple M5 Max.
Previous parses copied raw text. Typed measurements traverse the
parsed tree. The Go 1.26.10 comparison builds a generic bag tree,
then validates and projects the declaration or policy.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| Parse and project raw PEP declaration | 37.9 ns / 592 B | 160 ns / 912 B | |
| Parse and project raw PEP-Info policy | 39.2 ns / 616 B | 215 ns / 1,128 B | |
| Parse and project typed PEP declaration | | 136 ns / 832 B | 470–490 ns / 1,024 B |
| Parse and project typed PEP-Info policy | | 193 ns / 1,056 B | 636–643 ns / 1,320 B |
| Render parsed PEP through its builder | 13.7 ns / 264 B | 16.5 ns / 312 B | |
| Render parsed PEP as typed field lines | | 11.4 ns / 128 B | |

Core confirms that list decoding specializes to the declaration parser
or policy parser. Mapping URIs, policy identifiers and realm targets
share one URI scanner. The optimized PEP grammar object shrank from
616 KiB to 444 KiB when those call sites shared the scanner. Standalone
builder rendering still costs more than rendering the former raw value.


## PICS and JEPI protocol directives

Protocol and Protocol-Request distinguish the registered PICS 1.1
forms from the August 1996 PEP directive grammar. The PICS request
codec projects label completeness, extension groups, and the required
list of quoted service URLs. It retains future alphanumeric completeness
values and percent escapes in quoted extension names. PICS response
declarations identify the PICS-Label field. The general PEP directives
support requests and responses. [PICS 1.1](https://www.w3.org/TR/REC-PICS-labels-961031),
[August 1996 PEP draft, section 3.5](https://www.w3.org/TR/WD-http-pep-960820.html).

The shared PEP directive codec covers URI and token identifiers, typed
scope, strength, associated header patterns, nested parameters, and
realm references with optional trailing wildcards. It validates which
fields accept delegation or each strength level. Hermes rejects duplicate
reserved attributes, whose semantics the draft leaves undefined. All
four fields accept comma lists and the whitespace-separated directive
bags shown in JEPI examples. They retain accepted physical lines and
exact wire spelling. [JEPI wire examples](https://www.w3.org/TR/WD-jepi-uppflow).

The GHC 9.10.3 run passed 1,013 Hermes tests and 65 focused HTTP client
tests. An independent Go parser agreed on the canonical values and
wire bytes re-emitted by the header map for 54,299 cases, including
9,786 accepted cases. The corpus covered generated directives, quoted
octet and delimiter mutations, URI references, and repeated field
lines.

Sequential Criterion measurements used GHC 9.10.3 on Apple M5 Max.
The previous Hermes codecs copied raw text. The new parse fixtures
force the decoded result. The Go 1.26.10 parser validates a generic
bag tree to produce the equivalent view. The Go PICS response fixture
checks a separately backed copy of the literal and returns a cached result.

| Fixture | Previous Hermes raw | Current Hermes typed | Go |
| --- | ---: | ---: | ---: |
| Parse PICS response | 38.2 ns / 560 B | 6.8 ns / 48 B | 2.28–2.37 ns / 0 B |
| Parse PICS request | 39.5 ns / 592 B | 186 ns / 944 B | 674–683 ns / 1,104 B |
| Parse JEPI Protocol-Info | 37.9 ns / 624 B | 260 ns / 1,432 B | 1,085–1,194 ns / 1,480 B |
| Parse JEPI Protocol-Query | 39.8 ns / 576 B | 152 ns / 920 B | 656–693 ns / 728 B |
| Render parsed JEPI Protocol-Info as field lines | | 10.9 ns / 128 B | |

Core showed the PICS token scanner rebuilding the character set at
each call site. Sharing it reduced the typed request fixture from
624 to 186 ns and from 5,784 to 944 B. The
[PICS projection measurements](#pics-response-projection) below
update the canonical response comparison.
The JEPI query allocates more than its Go counterpart. Reruns of the
PEP and RFC 2774 fixtures after changing the shared field-list codec
measured the same or slightly lower allocation per operation for the
previously typed fixtures.


## PICS label lists

PICS-Label decodes service-level results, label-specific failures, and
individual and grouped ratings. It preserves category spelling and the
distinction between scalar and multiple values, including empty sets
and range endpoints. Each decimal retains its wire spelling and its
IEEE binary32 value. Conversion rounds tiny values to signed zero and
rejects overflow to infinity. Times retain the local date, minute and
signed GMT offset, including the specified minute 60.

The codec validates option aliases and uniqueness within each scope,
preserving comment order and distinct extension URLs. For each generic
label it checks for a local or inherited target URL. The label authoring
codec also checks nested extension data, MD5 value width, signature
Base64, and calendar dates. Quoted names and URLs use the same scanners
as the PICS request codec. Received bytes pass through the header map.
[W3C PICS 1.1 label recommendation](https://www.w3.org/TR/REC-PICS-labels-961031).

The GHC 9.10.3 suite passed 1,017 Hermes tests and 65 focused HTTP client
tests. An independent Go implementation agreed on 45,441 wire cases
covering binary32 values, signed zero, subnormals, rounded underflow and
overflow, dates, scalar and multivalue ratings, wire retention, and
single field-line handling. The Hermes tests additionally cover service
and label errors, nested extension data, digest and signature values,
and inherited generic targets.

Sequential Criterion measurements used GHC 9.10.3 on Apple M5 Max.
The old parser copied raw text. The typed fixtures force service and
rating values, including timestamps and range endpoints. The Go 1.26.10
reference validates the same label fixtures through a generic tree.

| Fixture | Previous Hermes raw | Current Hermes typed | Go |
| --- | ---: | ---: | ---: |
| Parse a single rating | 38.6 ns / 592 B | 308 ns / 1,408 B | 521–540 ns / 1,024 B |
| Parse options, dates, and a range | 38.5 ns / 704 B | 1.01 µs / 5,320 B | 1.51–1.54 µs / 2,728 B |
| Render a parsed label field | 37.3 ns / 928 B | 4.7 ns / 32 B | |

The numeric worker returns an unboxed binary32 value on success.
The typed field renderer returns the cached wire bytes. Parsing still
allocates more than the Go reference, especially on the options and
range fixture.


## P3P and OPS profile fields

P3P parses URI references, compact policies, and extension directives.
The compact codec recognizes the case-sensitive vocabulary and its
per-token consent qualifiers. It retains unknown tokens for forwarding.
The effective policy view selects the first `CP` in field order and
considers each recognized claim and qualifier once. Typed authoring
checks directive names, URI references, and compact token spelling.
Received field lines pass through the header map in order with their
original bytes. [W3C P3P recommendation](https://www.w3.org/TR/P3P/).

GetProfile, SetProfile, and ProfileObject enforce the HTTP field-value
octets and singleton field-line cardinality. The typed byte accessors
preserve opaque values, including optional whitespace. The historical
OPS note describes the profile objects and leaves their HTTP wire
encoding unspecified. The legacy text patterns provide a safe display
of the same field values. [W3C OPS over HTTP note](https://www.w3.org/TR/NOTE-OPS-OverHTTP),
[RFC 9110 field values](https://www.rfc-editor.org/rfc/rfc9110.html#section-5.5).

The GHC 9.10.3 suites passed 1,022 Hermes tests and 65 focused HTTP
client tests. An independent Go implementation matched all 38,136
cases for accepted wire output, canonical P3P directives, and effective
compact policy results. The OPS checks include every possible octet at
each position of an unaligned field slice and randomized mixed octets.

Sequential Criterion measurements used GHC 9.10.3 on Apple M5 Max.
The previous Hermes parser copied a raw text value. The new P3P
fixtures force the parsed directives, and the OPS byte fixture goes
through typed header lookup. The Go 1.26.10 opaque fixture times the
byte validator directly.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| Parse P3P policy reference | 36.3 ns / 560 B | 60.0 ns / 552 B | 173–186 ns / 208 B |
| Parse P3P compact policy | 40.0 ns / 616 B | 247 ns / 1,448 B | 482–534 ns / 1,056 B |
| Render P3P field lines | | 11.4 ns / 128 B | |
| Render P3P standalone builder | 12.1 ns / 264 B | 16.3 ns / 312 B | |
| Read ProfileObject as text | 35.7 ns / 552 B | 21.0 ns / 120 B | |
| Read ProfileObject as bytes | | 9.2 ns / 64 B | 7.6–8.1 ns / 0 B |
| Render ProfileObject | 12.9 ns / 264 B | 4.8 ns / 32 B | |

Core shows an unboxed cursor over eight-byte OPS blocks and a bounded
scalar tail. P3P dispatches known compact tokens on their packed
three-octet codes before falling back to slicing unknown tokens. The
typed P3P fixtures still allocate more than the Go comparison.


## Diagnostic fields

X-Request-ID, X-Correlation-ID, X-Trace-ID, and X-Powered-By now share
the opaque field-byte validator. Their byte accessors and both rendering
paths retain received optional whitespace and non-ASCII octets. Each
rejects repeated field lines. The legacy ShortText patterns still give
a safe display of the value. [RFC 9110 field values](https://www.rfc-editor.org/rfc/rfc9110.html#section-5.5).

X-Request-Start checks the optional `t=` prefix and reads signed,
integer, fractional, and exponent forms as an exact decimal. It
preserves received wire bytes including optional whitespace and exposes
checked wire and numeric constructors. Clients choose the epoch unit
according to their sender. [Heroku HTTP routing](https://devcenter.heroku.com/articles/http-routing),
[New Relic queue reporting](https://docs.newrelic.com/docs/apm/applications-menu/features/configure-request-queue-reporting/).

The GHC 9.10.3 suites passed 1,028 Hermes tests and 65 focused HTTP
client tests. A Python reference matched all 30,003 request-start
cases on grammar, exact coefficient and exponent, repeated fields,
wire retention, and numeric authoring.

Sequential Criterion and Go 1.26.10 measurements ran on Apple M5 Max.
The Go timing excludes header metadata and validates the byte fixture
or computes the same coefficient and scale for the nonnegative decimal
fixture. The older Hermes implementations returned raw text.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| Read X-Correlation-ID as text | 38.6 ns / 568 B | 29.1 ns / 136 B | |
| Read X-Correlation-ID as bytes | | 13.2 ns / 64 B | 11.7–15.4 ns / 0 B |
| Read X-Request-Start as text | 40.3 ns / 568 B | 36.6 ns / 392 B | |
| Read X-Request-Start as a decimal | | 25.8 ns / 328 B | 24.9–27.5 ns / 0 B |
| Render X-Request-Start field value | 18.9 ns / 280 B | 6.2 ns / 32 B | |

Core shows unboxed cursor and accumulator loops for the common decimal
forms. Signs, scientific notation, and coefficients beyond 18 digits
take the arbitrary-precision path.


## AMP cache transforms

AMP-Cache-Transform decodes the parameterised-list grammar from
draft-ietf-httpbis-header-structure-07. The codec keeps the original
field lines and combines them for parsing, including a quoted version
list that spans lines. It validates identifier syntax, rejects duplicate
parameters, and preserves bare parameters and typed string, integer,
float, and binary extension values. Floats keep their exact decimal
coefficient and scale. Binary values require proper Base64 padding.
[Structured Headers draft §3.3](https://datatracker.ietf.org/doc/html/draft-ietf-httpbis-header-structure-07#section-3.3).

Negotiation validates signed 64-bit version syntax and accepts
nonnegative versions. Reversed and overlapping intervals make the
affected alternative ineligible. Other parameters and malformed
version strings likewise let negotiation fall through to the next
alternative. Selection tries the ordered alternatives and returns the
highest offered version of the first match. Typed response authoring
emits the selected identifier and its exact version.
[AMP version negotiation](https://github.com/ampproject/amphtml/blob/main/docs/spec/amp-cache-transform.md#version-negotiation).

GHC 9.10.3 passed 1,034 Hermes tests and 65 focused HTTP client tests.
An independent Python codec agreed on 40,684 cases covering the draft
grammar, typed authoring, input field lines, and the selected response.
The AMP Packager's Go parser and version selector agreed on 12,009
negotiation cases in their shared grammar. The packager limits parameter
syntax to quoted values and ships a narrower transform version policy.
[AMP Packager at 57974efc943c](https://github.com/ampproject/amppackager/blob/57974efc943c/packager/amp_cache_transform/amp_cache_transform.go).

Sequential Criterion and Go 1.26.10 runs used Apple M5 Max. The
end-to-end fixture negotiates the same two alternatives against
versions 1 through 8 and returns the selected response bytes.

| Fixture | Previous Hermes | Current Hermes | AMP Packager |
| --- | ---: | ---: | ---: |
| Read alternatives and project text | 36 ns / 552 B | 89 ns / 1,032 B | |
| Parse and select a version | | 193 ns / 1,888 B | |
| Parse repeated lines and select a version | | 206 ns / 1,976 B | |
| Parse, select, and render the response field line | | 246 ns / 2,655 B | 911–947 ns / 1,304 B |
| Render a parsed field through the standalone builder | 13.5 ns / 264 B | 17.1 ns / 312 B | |
| Render parsed field lines for the header map | | 11.9 ns / 128 B | |

Core shows an unboxed offset and accumulator in the version digit loop.
Response authoring writes the selected version once and uses a slice of
the emitted field for the typed value. The end-to-end fixture still
allocates more than the AMP Packager.


## OSCORE and WebDAV If encoding follow-up

The OSCORE benchmark now uses the three octets `09 05 25` in both
languages and produces `CQUl`. The previous Hermes fixture contained
literal backslashes and decimal digits, which sent it through the
long-option encoder. The short encoder now writes one Base64url
quantum directly into a pinned byte array and exposes the unpadded
length. [RFC 8613 §11.2](https://www.rfc-editor.org/rfc/rfc8613.html#section-11.2).

The WebDAV If renderer emits punctuation as individual byte writes and
bulk-copies only the URI and entity-tag bytes. It still counts and
writes via one traversal definition so the exact buffer size follows
the canonical grammar. The typed tree retains the distinction between
tagged and untagged lists. [RFC 4918 §10.4.2](https://www.rfc-editor.org/rfc/rfc4918.html#section-10.4.2).

Sequential Criterion and Go runs used GHC 9.10.3 and Go 1.26.10 on
Apple M5 Max.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| Encode the same three OSCORE octets | 15.2 ns / 184 B | 12.7 ns / 136 B | 8.1–8.7 ns / 4 B |
| Render a WebDAV If condition tree | 85.2 ns / 272 B | 40.8 ns / 304 B | 24.4–25.0 ns / 64 B |

Core shows bounded indexed loads and direct pinned-array writes for
OSCORE. The Go reference returns a four-byte slice, while the Hermes
result owns a pinned array and foreign-pointer descriptor. WebDAV If
sizes the exact output and retains the nested typed input. The Go
fixture renders from a flattened slice into a 64-byte allocation.

All 1,034 Hermes tests and 65 focused HTTP client tests passed. The
38,200 WebDAV wire cases and the 308,110 binary-field cases matched
their references. An exhaustive check compared the empty OSCORE option
and every one-, two-, and three-byte option against Base64url, including
options backed by unaligned ByteString slices. All 16,843,009 matched.


## CORS singleton decoding follow-up

Access-Control-Request-Method uses the shared singleton token decoder
and renders directly from its compact text value. Its checked accessor
and constructor use the shared HTTP `Method` type, including custom
spellings and case-sensitive matching. Access-Control-Max-Age decodes
bare fields of up to nine digits directly. Longer values and values
with optional whitespace go through the existing saturation parser.
Both codecs enforce singleton fields. [Fetch CORS HTTP syntax](https://fetch.spec.whatwg.org/#http-new-header-syntax),
[RFC 9110 §9.1](https://www.rfc-editor.org/rfc/rfc9110.html#section-9.1),
[RFC 9111 §1.2.2](https://www.rfc-editor.org/rfc/rfc9111.html#section-1.2.2).

All 1,036 Hermes and 65 focused HTTP client tests passed. An independent
matrix matched 84,891 CORS and Timing-Allow-Origin cases. It includes
all byte substitutions for one- through fourteen-byte Max-Age fields,
method-token insertions, whitespace, overflow, and repeated lines.

Sequential Criterion and Go 1.26.10 runs used GHC 9.10.3 on Apple M5
Max. The method fixture returns an owned compact token in Hermes,
and an owned string in Go. The age fixture forces the parsed integer.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| Request-Method parse | 25.4 ns / 336 B | 15.5 ns / 72 B | 10.2–10.6 ns / 5 B |
| Max-Age parse | 19.5 ns / 272 B | 10.7 ns / 128 B | 7.9–8.5 ns / 0 B |

Core shows the decimal fold using an unboxed cursor and accumulator.
Token validation precedes one copy into a compact byte array. The
remaining per-field allocations include the byte-array descriptor for
the method and the boxed success result and buffer lifetime for the age.


## Trace Context encoding and decoding follow-up

Traceparent field rendering sizes a single output buffer and writes the
hex octets and delimiters directly. It copies the identifier text and an
optional opaque extension into that buffer. Tests compare the strict
header path with the standalone builder for normal identifiers and for
authored records with variable identifier lengths and sliced extensions.
[W3C traceparent format](https://www.w3.org/TR/trace-context-1/#traceparent-header).

Tracestate trims the value span before the copy into compact text, and
the list parser then consumes the trailing field whitespace. The Core
dump shows bounded eight-byte loads and the reverse scan staying within
the value span. The codec owns compact key and value arrays. The Go
fixture returns slices backed by the input and uses caller storage for
the two members. [W3C tracestate format](https://www.w3.org/TR/trace-context-1/#tracestate-header).

All 1,036 Hermes tests, 65 focused HTTP client tests, and 96,396
independent Trace Context parse and render cases passed.

Sequential Criterion and Go 1.26.10 runs used GHC 9.10.3 on Apple M5
Max. Earlier shared builder changes had already lowered traceparent
rendering to 23.7 ns before this pass.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| Traceparent parse | 41.9 ns / 496 B | 40.2 ns / 496 B | 30.8–34.6 ns / 0 B |
| Tracestate parse | 79.5 ns / 904 B | 72.2 ns / 728 B | 38.0–39.5 ns / 0 B |
| Traceparent field rendering | 23.7 ns / 280 B | 16.3 ns / 224 B | 19.8–21.0 ns / 64 B |


## Forwarding singleton follow-up

X-Forwarded-Port, Max-Forwards and Access-Control-Max-Age share a direct
decoder for one to nine bare decimal digits. The port codec checks the
Word16 bound before converting, and the normal parser handles whitespace
and larger inputs with the field's existing range rules. The HTTP and
HTTPS spellings have direct X-Forwarded-Proto codecs. The URI scheme
parser handles case variants and custom values.
[AWS forwarding fields](https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/x-forwarded-headers.html),
[RFC 9110 Max-Forwards](https://www.rfc-editor.org/rfc/rfc9110.html#section-7.6.2).

X-Real-IP uses a whole-field scan for a bare IPv4 address. It shares
the address scanner with the composable IPv4
parser, including the range and leading-zero rules. IPv6 and
whitespace-padded values use the field parser.
[RFC 3986 IPv4 grammar](https://www.rfc-editor.org/rfc/rfc3986.html#section-3.2.2).

X-Forwarded-For scans visible nodes in eight-byte blocks and checks the
remaining bytes with the shared ASCII scanner. The single field-line
parser constructs the nonempty list directly. Empty list slots,
repeated field ordering, and node spelling follow the existing codec.

Sequential Criterion and Go 1.26.10 runs used GHC 9.10.3 on Apple M5
Max. The Go X-Forwarded-For fixture borrows slices of the input and
uses caller storage. Hermes owns compact copies of the nodes. Core
shows unboxed decimal arithmetic and a packed IPv4 accumulator, and
the node scanner loads a word only when eight bytes are available.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| X-Forwarded-For | 70.0 ns / 592 B | 61.0 ns / 568 B | 32.6–32.9 ns / 0 B |
| X-Forwarded-Port | 18.2 ns / 272 B | 8.1 ns / 96 B | 5.6–5.8 ns / 0 B |
| X-Forwarded-Proto | 20.5 ns / 272 B | 9.5 ns / 48 B | 7.0–7.1 ns / 0 B |
| X-Real-IP | 32.3 ns / 384 B | 17.6 ns / 160 B | 12.3–12.4 ns / 0 B |
| Max-Forwards | 18.2 ns / 272 B | 8.0 ns / 112 B | 4.8–4.9 ns / 0 B |

All 1,036 Hermes tests and 65 focused HTTP client tests passed. The
forwarding codecs passed 131,756 typed parse and render cases, and the
shared address parser matched Go netip on 72,886 IPv6 and bracketed Host
cases. The shared decimal path also passed 84,891 CORS and Resource Timing
cases. The canonical Access-Control-Max-Age fixture took 11.0 ns and
allocated 128 bytes with the shared decoder.

## WebSocket version and offers follow-up

A bare one- to three-digit Sec-WebSocket-Version uses the shared decimal
decoder with the 0–255 bound and leading-zero rule. Short field lines with
one or two Sec-WebSocket-Protocol offers use the shared token character set
and copy the validated token spans into compact storage. The direct offer
scan is limited to 64 bytes. The RFC list parser handles other layouts,
including empty slots, longer offers, and repeated field lines. Response
protocol selection uses the single-token field decoder.
[RFC 6455 handshake grammar](https://www.rfc-editor.org/rfc/rfc6455.html#section-4.3).

Sequential Criterion runs used GHC 9.10.3 on Apple M5 Max. The Go fixture
keeps borrowed token slices in caller storage. Simplified Core shows
unboxed decimal arithmetic and bounded pointer loops, with one short
byte-array copy per retained offer token after scanning.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| Version `13` | 23.7 ns / 336 B | 9.2 ns / 104 B | 11.0–11.2 ns / 0 B |
| Protocol offers `chat, superchat` | 44.9 ns / 496 B | 27.9 ns / 304 B | 19.6–26.5 ns / 0 B |

All 1,036 Hermes tests and 65 focused HTTP client tests passed. The
independent parse and render corpus passed 90,312 cases, including byte
substitutions, case-sensitive duplicate detection, and values on both
sides of the offer scan limit.

## HTTP/1 Host and CONNECT authorities

The whole-block and streaming request paths use one Host validation step.
They require a field on HTTP/1.1 and check validity and singleton cardinality
whenever it is present. Empty Host is accepted, and the parser keeps the
received authority spelling. A single scan finds the field and checks for
duplicate lines. A bare registered name using ASCII letters, digits, dots,
and hyphens avoids the general parser. Hermes's Host decoder handles ports,
percent escapes, bracketed IPv6, and IPvFuture literals. CONNECT requires a
nonempty URI host and an explicit port from the same authority grammar.
[RFC 9112 request-target and Host rules](https://www.rfc-editor.org/rfc/rfc9112.html#section-3.2),
[RFC 9110 CONNECT](https://www.rfc-editor.org/rfc/rfc9110.html#section-9.3.6).

All 79 HTTP/1 tests passed, including a loopback request that reaches the
handler with a bracketed authority and one that gets a 400 for an invalid
port. All 1,036 Hermes tests and 65 focused HTTP client tests passed. A
separate 93,058-case authority corpus agreed with both request parsers, and
with typed Host lookup or CONNECT validation as applicable. External
h1spec and Http11Probe verification is pending.

Sequential Criterion runs used GHC 9.10.3 on Apple M5 Max. Core shows the
bare-host predicate running on unboxed bytes inside a bounded loop.

| Request parser fixture | Previous | Current |
| --- | ---: | ---: |
| Small GET | 131.3 ns / 1,312 B | 110.8 ns / 1,080 B |
| POST with 10 headers | 426.8 ns / 3,728 B | 395.8 ns / 3,496 B |

## HTTP/1 transfer framing

The request and response parsers locate framing fields in one pass.
A singleton chunked coding uses a literal fast path. Other transfer
coding lists pass through the Hermes codec in field order, including
quoted extension parameters. Requests with transfer codings require
final chunked framing. HTTP/1.1 responses with a body and no declared
length, or with a transfer coding other than chunked last, read to EOF.
The reader removes final chunk framing and delivers the remaining
transfer-coded bytes. The HTTP/1 pool discards close-delimited
connections, including when the peer omits the Connection field.
[RFC 9112 message body length](https://www.rfc-editor.org/rfc/rfc9112.html#section-6.3).

Content-Length uses Hermes's short-decimal and bounded Word64 parsers.
The transport accepts repeated equal decimal values, including comma
lists, and accepts the full Word64 range. EOF before the declared byte
count is a protocol error. The eager client reports that error in its
Either result. A server that already sent a final response closes if
draining the request body fails.

All 87 HTTP/1 tests passed. Loopback checks read a close-delimited body,
send a second pooled request on a fresh connection, and report a
truncated fixed-length response. An independent 7,305-case corpus
agreed with both request and both response parser entry points.
See [connection reuse](#http1-connection-reuse) for the aggregate pool.

Sequential Criterion runs used GHC 9.10.3 on Apple M5 Max. Core shows
one header-list traversal and unboxed decimal accumulation.

| Request parser fixture | Before framing changes | After framing changes |
| --- | ---: | ---: |
| Small GET | 110.8 ns / 1,080 B | 110.4 ns / 1,064 B |
| POST with 10 headers | 395.8 ns / 3,496 B | 376.0 ns / 3,320 B |

## HTTP/1 connection reuse

The HTTP/1 reader records whether the parsed message permits another
exchange. Both client pools use that state after the response body is
drained. Close-delimited responses, a `close` option on any Connection
field line, protocol changes, and framing read errors retire the
connection. HTTP/1.0 responses require an explicit keep-alive option
to permit reuse. The same state tracks idempotent transport teardown.
TCP sockets opened by the HTTP/1 client are closed when the connection
is released. Callers that supply their own socket retain ownership.
[RFC 9112 persistence rules](https://www.rfc-editor.org/rfc/rfc9112.html#section-9.3).

All 311 aggregate HTTP tests, 88 HTTP/1 tests, and 1,036 Hermes tests
passed. Loopback tests cover close-delimited responses without a
Connection field, a `close` option on a later line, and release of the
client's TCP socket. An initial aggregate run timed out on the HTTP/2
parallel POST case. Isolated repeats and subsequent full runs passed.

The aggregate benchmark fixtures now initialize the response push
promise field. All three HTTP/1 fixtures completed a 50-iteration
Criterion smoke run. A timed run hit a local socket-address error.

## HTTP/1 trailer fields

The client and server use the same chunked writer, which runs the
trailer action after the body producer reaches EOF. Received trailers
use the HTTP/1 field-line grammar and are copied before the receive
ring advances. Repeated fields stay in wire order and separate from
the initial headers. The aggregate adapter preserves spelling while
constructing case-insensitive names. The reader caps each line at
8 KiB of field bytes and the entire trailer section at 32 KiB, matching
the existing header section limit. The line cap applies to the current
line even when later lines are already buffered.
[RFC 9112 trailer section](https://www.rfc-editor.org/rfc/rfc9112.html#section-7.1.2),
[RFC 9110 trailer processing](https://www.rfc-editor.org/rfc/rfc9110.html#section-6.5.2).

Loopback tests deliver body-derived Content-Digest trailers in both
directions, then decode the Trailer announcement and digest through
Hermes. The HTTP/1 tests also retain repeated values beyond connection
teardown and exercise both size limits. All 91 HTTP/1 and 1,036 Hermes
tests passed. All 312 aggregate cases passed with synchronous test
scheduling. The concurrent run again timed out on the HTTP/2 parallel
POST case. The three HTTP/1 Criterion fixtures passed a 50-iteration
smoke run.

## HTTP/2 trailer fields

The aggregate client passes request trailers through all three send
entry points. The HTTP/2 request and response writers evaluate a
streaming body's trailer action once, after its producer reaches EOF.
They send nonempty trailers in a final HEADERS block with END_STREAM.
An empty trailer list uses the empty DATA terminator for streamed
bodies. Fixed request bodies and empty requests can also carry a
trailer block. Existing low-level response trailer lists still work.
[RFC 9113 message framing](https://www.rfc-editor.org/rfc/rfc9113.html#section-8.1).

Loopback tests decode typed Content-Digest trailers in both directions,
keep repeated extension fields in order, and check that a throwing
streaming trailer action resets its stream. The connection remains
usable after a reset. All 315 aggregate HTTP tests passed with both
synchronous and concurrent test scheduling. All 80 HTTP/2 engine tests
and 1,036 Hermes tests passed. Simplified GHC Core puts the trailer
action in the EOF branch of both body loops. The three HTTP/2 Criterion
fixtures completed a 50-iteration smoke run.

## HTTP/2 trailer validation

Both receivers validate the decoded trailer block before delivering
it. They require END_STREAM, lowercase token field names, and valid
field values. Pseudo-headers and connection-specific fields cause a
stream-level PROTOCOL_ERROR. HPACK decoding still updates the shared
compression table before semantic validation. The server checks the
request Content-Length before publishing trailers, and a rejected
trailer block surfaces through the body and trailer readers.
[RFC 9113 message framing and validation](https://www.rfc-editor.org/rfc/rfc9113.html#section-8.1).

The client waits for the final response after informational headers.
A loopback test covers 103 Early Hints followed by a 200 response, DATA,
and a trailer block. Unit checks include all 256 octets in a trailer
field name. Simplified GHC Core shows unboxed byte scans in the validator.
All 87 HTTP/2 engine tests, 1,036 Hermes tests, and 315 aggregate HTTP
tests passed. The aggregate suite passed under both test schedules.

Sequential Criterion runs on GHC 9.10.3 put the HTTP/2 GET fixture at
368–391 µs after the receive changes and 376–389 µs before. Outliers
varied across the 64 KiB POST and streaming fixtures, so those runs
leave the performance comparison open.

## HTTP/2 Content-Length

The client and server read message lengths with Hermes's checked decimal
parsers and a Word64 byte budget. Repeated values are accepted only
when they agree, including comma-combined values. Malformed, conflicting,
or overflowing lengths fail the stream. The readers subtract unpadded
DATA bytes and check for a zero remainder when HEADERS, DATA, or
trailers carry END_STREAM. Flow-control accounting includes padding.

HEAD and 304 responses can carry a representation length without a
body. Informational and 204 responses forbid Content-Length. A 205
response requires zero content. The client ignores Content-Length
for framing a successful CONNECT tunnel.
[RFC 9110 Content-Length](https://www.rfc-editor.org/rfc/rfc9110.html#section-8.6),
[RFC 9113 malformed messages](https://www.rfc-editor.org/rfc/rfc9113.html#section-8.1.1).

Unit tests cover syntax, repeated lengths, Word64 bounds, and the
byte budget at END_STREAM. Loopback tests cover matching declared
lengths with DATA and trailers, plus HEAD and 304 metadata followed
by a regular request on the same connection. All 91 HTTP/2 engine
tests, 1,036 Hermes tests, and 316 aggregate HTTP tests passed.
The aggregate suite passed with synchronous and concurrent scheduling.

Simplified Core inlines the remaining-length check into both DATA
paths and uses unboxed Word64 arithmetic. Two sequential Criterion
runs completed all three HTTP/2 fixtures. The run-to-run spread,
especially on the streaming fixture, leaves the timing comparison open.

## Initial HTTP/2 field validation

Request heads, response heads, and trailers use the same field-name
and field-value checks. The request validator checks pseudo-header
ordering and uniqueness, then validates the regular fields. The client
also checks decoded push promises against the request grammar before
making them available to callers. The HTTP/2 request TE field accepts
only the case-insensitive trailers keyword. Response and trailer blocks
reject TE, and all three sections reject connection-specific fields.
Field grammar violations fail their stream after HPACK decoding.
[RFC 9113 fields and pseudo-headers](https://www.rfc-editor.org/rfc/rfc9113.html#section-8.2).

Classic CONNECT sends only :method and :authority. The receiver uses
Hermes's authority grammar to check that an explicit host and port
are present. The unified HTTP API maps that authority back into the
request target. Loopback tests cover this mapping, request TE, and
the final response following Early Hints.
[RFC 9113 CONNECT](https://www.rfc-editor.org/rfc/rfc9113.html#section-8.5).

All 97 HTTP/2 engine tests and 1,036 Hermes tests passed. All 317
aggregate HTTP tests passed under both test schedules. Simplified
Core shows unboxed byte scans in the shared validator and an unboxed
request pseudo-header mask.

Two sequential Criterion runs completed all three HTTP/2 fixtures.
The GET estimates were 366–397 µs. The two preceding runs measured
375–379 µs. Outliers and run-to-run spread limit the comparison.

## HTTP/2 authority and Host

The request head uses Hermes's authority grammar to validate ordinary
:authority and Host values. Host is a singleton. It supplies the
authority when the pseudo-header is absent. When both are present,
the comparison accounts for host case, default HTTP and HTTPS ports,
unreserved percent escapes, and equivalent IPv6 addresses. It retains
the selected authority's original wire spelling. The client validates
the head before allocating a stream and emits :authority when Host
supplies it. HTTP/2 requests forwarded through the unified HTTP/1
adapter use the selected authority for the outgoing Host field.
[RFC 9113 request pseudo-headers](https://www.rfc-editor.org/rfc/rfc9113.html#section-8.3.1),
[RFC 9110 HTTP URI normalization](https://www.rfc-editor.org/rfc/rfc9110.html#section-4.2.3).

Loopback tests cover Host-derived authority, client rejection before
stream allocation, and the selected authority reaching an HTTP/1
origin through an HTTP/2 intermediary. All 102 HTTP/2 engine tests,
1,036 Hermes tests, and 318 aggregate HTTP tests passed synchronously.
One concurrent run timed out in the interleaved-stream test. Three
concurrent reruns passed all 318 cases. An earlier build timed out
once on the distinct-header HPACK stress case and passed two reruns.

Simplified Core shows unboxed byte offsets in the case-folded host
comparison. Two sequential Criterion runs completed all three HTTP/2
fixtures. The GET estimates were 408–419 µs, versus 366–397 µs in the
preceding runs. The loopback measurements leave the cost of the
additional validation open.

### Authority parser performance

The request-head parser checks method, scheme, Host, and :authority
with their specific grammars. Ordinary host names and short decimal
ports use one bounded host scan and Hermes's short decimal parser.
IP literals, escaped names, and other spellings use the full authority
parser. Simplified Core shows an unboxed offset in the host scan
and no intermediate optional index.

An acceptance corpus agreed with the full Hermes authority parser on
397,584 inputs. It covered six host forms, every decimal port from
0 through 66,000, longer spellings, and non-digit port bytes. All
102 HTTP/2 engine tests, 1,036 Hermes tests, and 318 aggregate HTTP
tests passed. The aggregate suite passed under both test schedules.

Sequential Criterion runs on GHC 9.10.3 compared saved before and after
binaries parsing a complete GET header list and forcing the selected
authority. Allocation estimates come from the RTS allocation counter.

| Fixture | Time before | Time after | Bytes before | Bytes after |
| --- | ---: | ---: | ---: | ---: |
| Host name | 101–107 ns | 87–92 ns | 272 | 272 |
| Host and port | 113–134 ns | 85–87 ns | 568 | 400 |
| Equivalent Host and :authority | 204–209 ns | 136–142 ns | 624 | 456 |
| Bracketed IPv6 and port | 181–186 ns | 166–167 ns | 1,272 | 1,272 |
| Escaped name matching Host | 189–196 ns | 156–164 ns | 736 | 736 |

Paired loopback GET measurements used the saved builds in alternating
order. Estimates were 380–383 µs before and 370–385 µs after, with
overlapping confidence intervals. The three HTTP/2 loopback fixtures
also passed a 50-iteration smoke run.

## gRPC engine header receive paths

The gRPC engine server uses the shared HTTP/2 request head parser,
including Host-derived authority and the regular field grammar. The
client uses the shared status parser and response field grammar. It
waits for the final response after informational headers. Both
receive paths validate terminating trailer fields and signal
malformed blocks as stream failures. The client publishes an
initial END_STREAM response's trailer metadata before waking the
response waiter, preserving the gRPC trailers-only layout.
[RFC 9113 message framing](https://www.rfc-editor.org/rfc/rfc9113.html#section-8.1),
[gRPC over HTTP/2](https://github.com/grpc/grpc/blob/master/doc/PROTOCOL-HTTP2.md#responses).

The engine fixtures cover Early Hints followed by a response and
trailers, immediate trailers-only gRPC metadata, and Host-derived
request authority with terminating request trailers. All 105 HTTP/2
engine tests, 1,036 Hermes tests, and 318 aggregate HTTP tests passed.
The aggregate suite passed under both schedules. The minimal gRPC
suite passed its framing, tracer, and code generation build checks.

The full gRPC suite passed 91 tests, with the existing ProtoLens
label test marked pending. Coverage includes gRPC status on a
trailers-only reply, reuse after a failed call, end-of-stream handling,
streaming RPCs, and cancellation.

## gRPC HTTP/2 fragmented field blocks

The gRPC engine assembles request heads, response heads, and trailers
across HEADERS and same-stream CONTINUATION frames. It removes padding
and priority data from the initial fragment, preserves END_STREAM from
HEADERS, and concatenates fragmented HPACK input once. The fragmented
wire-byte budget is 16 MiB, including frame headers. The single-frame
receive path passes its HPACK bytes directly to the decoder. Outgoing
header fragments share one send lock, and outgoing HEADERS and DATA
respect the peer's negotiated maximum frame size.
[RFC 9113 CONTINUATION frames](https://www.rfc-editor.org/rfc/rfc9113.html#section-6.10).

The scripted fixtures cover padded and prioritized initial fragments,
fragmented trailers-only responses, and request and response trailers.
An engine-to-engine fixture exchanges 30,000-byte metadata values and
48,000-byte DATA payloads. All 106 HTTP/2 engine tests, 1,036 Hermes
tests, and 318 aggregate HTTP tests passed synchronously. The minimal
gRPC suite passed, and the full suite passed 91 tests with the existing
ProtoLens label test marked pending. Simplified Core shows an unboxed
remaining-byte counter and one concatenation on the fragmented path.

## HTTP/2 padded and fragmented field blocks

The ordinary HTTP/2 client and server use the shared field-block
assembler for request heads, response heads, and trailers. The
client also assembles PUSH_PROMISE blocks before decoding them.
Frame-specific prefixes and padding are removed from the first
fragment, and the initial HEADERS flags retain END_STREAM through
assembly. The receive loop reads only same-stream CONTINUATION
frames until the block ends. The server still applies its
header-block deadline during assembly.
[RFC 9113 HEADERS](https://www.rfc-editor.org/rfc/rfc9113.html#section-6.2),
[PUSH_PROMISE](https://www.rfc-editor.org/rfc/rfc9113.html#section-6.6),
[CONTINUATION](https://www.rfc-editor.org/rfc/rfc9113.html#section-6.10).

Each ring-backed fragment is copied before the next receive step.
The assembler joins the fragments once, and single-frame blocks
skip assembly. Simplified Core shows one bulk copy per retained
slice and an unboxed remaining-byte counter in the assembly loop.
The loopback fixtures exercise padded and prioritized request and
response heads, terminating trailers, and a padded push promise
followed by its response.

All 108 HTTP/2 engine tests, the TLS smoke test, 1,036 Hermes tests,
and 318 synchronous aggregate HTTP tests passed. The minimum gRPC
suite passed, and the full gRPC suite passed 91 tests with one
existing pending case. The concurrent aggregate suite passed one of
three runs. The other runs each timed out in one case, respectively
HTTP/1 rapid requests on a persistent connection and HTTP/2 interleaved
streams. All eight focused concurrency tests passed ten consecutive runs.

Two pairs of sequential Criterion runs used saved before and after
binaries of the aggregate loopback benchmark. The 64 KiB POST echo
estimates went from 522–532 µs to 484–486 µs. The GET and streaming
response confidence intervals overlapped. Allocations rose by about
2–4 KiB per GET and POST and 13–16 KiB per streaming response.

## gRPC engine Content-Length

The gRPC HTTP/2 engine validates request and response Content-Length
using the ordinary HTTP/2 parser and response status rules. The stream
tracks a declared length in bytes and checks for completion at
END_STREAM, whether the message ends on its initial field block,
DATA, or trailers. DATA padding is removed before counting or
delivering content. Connection flow-control credit still uses the
entire DATA frame payload, including padding and discarded bytes.
HEAD and 304 retain the representation length in the head and report
an empty body. The engine exposes a declared body size when it fits
the public Int-sized accessor.
[RFC 9113 message framing](https://www.rfc-editor.org/rfc/rfc9113.html#section-8.1.1),
[RFC 9113 flow control](https://www.rfc-editor.org/rfc/rfc9113.html#section-6.9.1),
[RFC 9110 Content-Length](https://www.rfc-editor.org/rfc/rfc9110.html#section-8.6).

Engine fixtures exercise padded DATA followed by trailers in both
directions, a trailers-only response with length zero, HEAD and 304
responses, and 48,000-byte request and response bodies split across
frames. The 109 HTTP/2 engine tests, TLS smoke test, 1,036 Hermes tests,
and 318 aggregate HTTP tests passed. The aggregate suite passed under
both schedules. The minimal gRPC suite passed, and the full gRPC
suite passed 91 tests with one existing pending test. Simplified Core
shows unboxed 64-bit comparisons and subtraction for declared lengths.

Two pairs of sequential aggregate HTTP loopback benchmarks exercised
the ordinary HTTP/2 client with the shared helpers. The repeated runs
had overlapping timing confidence intervals for all three fixtures.
Allocation estimates differed by less than one percent. These runs
measure the ordinary HTTP stack.

## gRPC engine outbound trailers

The HTTP/2 callback engine shares one trailer writer across requests
and responses. It supports bodyless, builder, and both streaming APIs.
Each chunk advances the maker to its successor. Streaming trailers are
computed after the body callback returns, since applications can make
the final metadata available after the final push. The writer validates
and sends the fields in order on a terminating HEADERS block. Producer
failures reset the affected stream. A local request producer failure
also reaches the waiting client callback.
[RFC 9113 trailer framing](https://www.rfc-editor.org/rfc/rfc9113.html#section-8.1).

The default constructors carry an explicit absence of a trailer maker.
They put END_STREAM on the final DATA chunk, preserving gRPC final
message detection. An explicit final push makes cancellation a no-op
while the writer finishes an installed maker. Existing OutObj record
construction and updates compiled in a separate client module.

The engine fixtures cover stateful makers, body-derived fields in wire
order, fragmented 30,000-byte trailers in both directions, and
connection reuse after a failing producer. All 116 HTTP/2 engine tests,
the TLS smoke test, 1,036 Hermes tests, and 318 aggregate HTTP tests
passed. The aggregate suite passed under both schedules. The full gRPC
suite passed 91 tests with one existing pending test. The minimal suite
also passed.

A local Criterion harness compared saved before and after binaries
over the in-memory engine transport. The no-trailer 1 KiB final-push
fixture went from 24.8–25.7 µs to 24.1–24.5 µs. Timing intervals
overlapped on the headers-only, fixed-body, and two-chunk fixtures.
Allocation rose by about 150–660 bytes per request across the four
fixtures, approximately one percent or less.

## IPv6 Host scanning

IPv6 validation and numeric decoding share a bounded byte scanner.
The validation path keeps digit and group counts, and the numeric
path also packs the groups into two words. Embedded IPv4 tails reuse
the dotted-quad reader. The Host codec still retains the literal's
original spelling, and the authority grammar still accepts IPvFuture.
Bare IPv6 parsing and X-Real-IP use the numeric scanner directly.
[RFC 3986 host grammar](https://www.rfc-editor.org/rfc/rfc3986.html#section-3.2.2).

Go netip agreed on IPv6 acceptance and all 128 decoded bits across
89,270 fixtures. The Host codec accepted the same addresses and kept
their original spelling. The corpus adds 16,384 mixed-address forms
with varied compression positions.
The 1,036 Hermes tests, 116 HTTP/2 engine tests, TLS smoke test, and
318 aggregate HTTP tests passed. The aggregate suite passed under
both schedules. The minimal gRPC suite passed, and the full gRPC
suite passed 91 tests with one existing pending test. GHC 9.10.3 Core
shows unboxed address arithmetic and counters in the validation loop.

Sequential Criterion measurements used the checked-in bracketed Host
fixture and an additional harness for mixed and full addresses. The
additional harness's before build uses the hex-only byte scanner and
the earlier dotted-tail fallback. Go netip returns a fixed-size
numeric value. Hermes Host retains a compact copy of the wire spelling.

| Fixture | Before | After | Go netip |
| --- | ---: | ---: | ---: |
| Compressed hex Host and port | 113–115 ns / 1,136 B | 45–46 ns / 432 B | 31–44 ns / 0 B |
| IPv4-mapped Host and port | 188–189 ns / 1,904 B | 46–52 ns / 424 B | 32–35 ns / 0 B |
| Eight-group Host and port | 74–78 ns / 448 B | 75 ns / 448 B | 44–45 ns / 0 B |
| Bare mixed IPv6 | 252–258 ns / 2,688 B | 51–54 ns / 512 B | 26–28 ns / 0 B |

## PICS response projection

The Protocol singleton decoder and the retained-list accessor inline
through the cached PICS response. The generic parser and legacy text
reparse have separate entry points. GHC 9.10.3 Core reduces the
matched typed branch to a byte comparison followed by the cached
ProtocolPicsLabels value, eliminating intermediate Either values.
[PICS 1.1 response grammar](https://www.w3.org/TR/REC-PICS-labels-961031).

The PICS benchmarks copy their input outside the timed operation,
matching the Go reference. The typed response went from 6.9 ns and
48 B to 5.0 ns and 32 B. The spelling projection went from 22.4 ns
and 112 B to 6.1–6.2 ns and 48 B. Go took 2.32–2.44 ns and allocated
0 B, so the typed fixture still sits slightly above the 2× target.
Experimental word comparisons measured 5.8–10.7 ns. The shared list
accessor also reduced allocation by 16 B on the typed PICS request,
JEPI Protocol-Info, and JEPI Protocol-Query fixtures.

The independent PICS and JEPI corpus agreed on acceptance, typed
entries, and emitted field lines for all 62,235 cases. It includes
every octet substitution at each byte of the canonical declaration.
All 1,036 Hermes, 116 HTTP/2, and 318 aggregate HTTP tests passed.
The aggregate suite passed under both schedules, and the TLS smoke
and minimal gRPC suites passed. The full gRPC suite passed 91 tests
with one existing pending test.

## Structured Fields parsing and canonical rendering

The shared Structured Fields codec joins repeated field lines with
comma-space. If a quoted String, Display String, or String parameter
spans a line boundary, the inserted space is part of the decoded value.
The List parser recognizes an empty value after leading spaces and
accepts horizontal tabs as optional whitespace after a member and
around a delimiter. A tab before the first member fails parsing.
The older AMP draft codec keeps its comma-only join for split version
constraints.
[RFC 9651 §4.2](https://www.rfc-editor.org/rfc/rfc9651.html#section-4.2),
[RFC 9110 §5.3](https://www.rfc-editor.org/rfc/rfc9110.html#section-5.3).

The HTTP Working Group corpus exercised 1,607 Item, List, and Dictionary
parse fixtures. The harness compared typed values and canonical
serialization for every accepted fixture. Hermes matched all required
outcomes and rejected the optional partially padded and extra-padding
Base64 fixtures. The prior build differed on the two split-string values.
Five supplemental cases covered whitespace boundaries and a split List
parameter. The local regression suite also exercises the change through
typed Cache-Status lookup and rendering.
[HTTPWG fixture corpus](https://github.com/httpwg/structured-field-tests/tree/4a4782f8234512af133f6f57c4ca998578b57110).

Sequential Criterion runs on GHC 9.10.3 put the before and after samples
within overlapping timing intervals for Cache-Status, Proxy-Status, and
repeated Priority. Cache-Status and Proxy-Status each allocated 16 fewer
bytes per operation. Repeated Priority allocated about 1,888 bytes per
operation in both runs.

All 1,039 Hermes, 116 HTTP/2, and 318 aggregate HTTP tests passed. The
aggregate suite passed under both schedules. The TLS smoke and minimal
gRPC suites passed. The full gRPC suite passed 91 tests with one existing
pending test.

## Omitting empty Structured Fields

Empty Permissions-Policy and Reporting-Endpoints dictionaries and empty
Save-Data and Sec-Purpose lists omit the field on output. The public
`renderHeaderLines` applies field-specific omission and converts the
cardinality-specific render result to actual field lines. Calling
`setHeader` with one of these values clears any existing lines for its
field name. An empty outer Permissions-Policy dictionary and an empty
inner allowlist remain distinct. An empty Accept-CH sends a blank field
line, the browser signal to clear the origin's persisted hints.
[RFC 9651 §4.1](https://www.rfc-editor.org/rfc/rfc9651.html#section-4.1),
[Permissions Policy](https://w3c.github.io/webappsec-permissions-policy/#permissions-policy-http-header),
[Client Hints Infrastructure](https://wicg.github.io/client-hints-infrastructure/).

The cardinality-specific render API still exposes a ByteString for the
three singleton renderers. The map write for an ordinary Content-Length
specializes without an omission check in GHC 9.10.3 Core. Repeated
Criterion samples measured Reporting-Endpoints map writes with a fixed
value at 19.4 ns before and 19.6 ns after, both 208 bytes. The corresponding
Permissions-Policy map write went from 20.8 ns and 232 bytes to 22.4 ns
and 240 bytes. The checked-in Permissions-Policy byte-rendering fixture
went from 84.9 to 85.8 ns with overlapping confidence intervals and
allocated eight more bytes.

All 1,040 Hermes, 116 HTTP/2, and 318 aggregate HTTP tests passed on the
final build. Aggregate HTTP passed under both schedules. The TLS smoke
and minimal gRPC suites passed. The full gRPC suite passed 91 tests with
one existing pending test.

## Link targets and parameters

The Link codec validates the URI-reference inside each pair of angle
brackets and retains its spelling, including relative references and
the empty reference. It combines repeated field lines in order and
skips empty list slots. Quoted parameters follow HTTP quoted-string
grammar, using a one-to-one Latin-1 mapping to retain their wire octets.
[RFC 8288 §3](https://www.rfc-editor.org/rfc/rfc8288.html#section-3),
[RFC 9110 §5.6.1](https://www.rfc-editor.org/rfc/rfc9110.html#section-5.6.1).

The parameter list retains duplicates and original names for forwarding.
The typed relation and anchor views use the first case-insensitive match.
Relation values distinguish registered-form names from absolute extension
URIs. The title view prefers a decodable RFC 8187 `title*` value and can
fall back to `title`. URI resolution needs the caller's representation
URL. [RFC 8288 §3.1–3.4](https://www.rfc-editor.org/rfc/rfc8288.html#section-3.1).

All 38,603 independently generated RFC 3986 fixtures agreed on typed
acceptance, target spelling, and field rendering. New package tests
check repeated lines, registered and extension relations, the title
fallback, and every octet in quoted-pair position. Simplified GHC
9.10.3 Core shows the URI and quote scans testing unboxed addresses.

Sequential Criterion measurements projected the target and parameter
lengths or forced the rendered field. The local Go comparison uses
ordered parameters and validates fixture targets with `net/url`.

| Fixture | Previous Hermes | Current Hermes | Go comparison |
| --- | ---: | ---: | ---: |
| Parse a link with a query and an escaped title | 367 ns / 3,744 B | 169 ns / 1,224 B | 305–319 ns / 504 B |
| Parse three links across three field lines | 634 ns / 5,616 B | 394 ns / 2,824 B | 880–899 ns / 1,256 B |
| Render the link with an escaped title | 120 ns / 864 B | 107 ns / 760 B | 97–99 ns / 176 B |

All 1,044 Hermes, 116 HTTP/2, and 318 aggregate HTTP tests passed. Aggregate
HTTP passed under both schedules. The TLS smoke and minimal gRPC suites
passed. The full gRPC suite passed 91 tests with one existing pending
test.

## Link rendering

Typed Link rendering counts and fills one exact-sized output buffer. The
composable builders reserve space in their destination and use the same
writer. Counting and writing share the traversal for delimiters, compact
text, and escaped octets. The quoted body keeps the Latin-1 mapping for
parsed values and the existing UTF-8 behavior for higher authored Unicode
code points. [RFC 9110 §5.6.4](https://www.rfc-editor.org/rfc/rfc9110.html#section-5.6.4).

A differential run compared the previous quote renderer against all
1,112,064 Unicode scalar values on three paths. It checked typed field
rendering and both builders composed between other fragments. The
package regression also covers a quoted value larger than the builder's
initial buffer. GHC 9.10.3 Core shows unboxed octet reads and writes.

Sequential Criterion measurements used the checked-in Link fixtures.
The composed field fixture compares the previous builder expression
with the shared writer. The Go comparison for the escaped title from
the previous pass took 95–100 ns and allocated 176 bytes.

| Fixture | Previous | Current |
| --- | ---: | ---: |
| Render a preload link | 40.7 ns / 328 B | 38.1 ns / 296 B |
| Render a link with an escaped title | 103.3 ns / 760 B | 77.2 ns / 344 B |
| Render three links | 172.8 ns / 1,215 B | 111.6 ns / 520 B |
| Compose an escaped-title Link field in a larger builder | 116.1 ns / 712 B | 79.0 ns / 376 B |

All 1,045 Hermes, 116 HTTP/2, and 318 synchronous aggregate HTTP tests
passed. The final concurrent aggregate suite passed two of three runs.
The other run timed out in the previously observed HTTP/2 interleaved
streams test. The eight focused concurrency tests passed across seeds
42–52. The TLS smoke and minimal gRPC suites passed. The full gRPC suite
passed 91 tests with one existing pending test.

## Prefer and Preference-Applied

Both RFC 7240 codecs accept whitespace around equals signs, combine
repeated lines in order, and skip empty list positions. The combined
list requires at least one preference. Quoted words retain their wire
octets and original escapes. `lookupPreference` applies the first
case-insensitive name match on requests. `decodePreferenceValue` removes
quoting and treats an absent value and an empty quoted string alike.
Prefer accepts optional semicolon parameter slots. Preference-Applied
rejects semicolon parameters and emits the response grammar when the
legacy constructor's parameter field is populated.
[RFC 7240 §2](https://www.rfc-editor.org/rfc/rfc7240.html#section-2),
[RFC 7240 §3](https://www.rfc-editor.org/rfc/rfc7240.html#section-3).

An independent grammar matched typed acceptance and rendering on
23,432 cases, including repeated field lines and every octet in quoted
and quoted-pair positions. Package tests cover the same wire paths,
the first occurrence rule, and decoded empty and opaque values.
Simplified GHC 9.10.3 Core uses unboxed address reads in the quoted-word
scanner. Shared Latin-1 helpers also serve Refresh, whose three
checked-in fixtures kept their allocation counts and had overlapping
timing intervals.

Sequential Criterion fixtures project member and parameter lengths or
force the rendered field. The local Go parser retains slices into the
input and stores members in slices. Hermes copies words into compact
text and uses ordered lists in its public representation.

| Fixture | Previous Hermes | Current Hermes | Go comparison |
| --- | ---: | ---: | ---: |
| Parse Prefer with a quoted extension parameter | 452 ns / 4,600 B | 137 ns / 1,024 B | 243–256 ns / 432 B |
| Parse repeated Prefer field lines | 308 ns / 2,920 B | 175 ns / 1,880 B | 268–272 ns / 432 B |
| Render Prefer | 153 ns / 1,376 B | 58 ns / 328 B | 73–78 ns / 240 B |
| Parse Preference-Applied | 104 ns / 920 B | 75 ns / 696 B | 109–120 ns / 176 B |
| Render Preference-Applied | 95 ns / 1,016 B | 29 ns / 328 B | 31–34 ns / 48 B |

All 1,048 Hermes, 116 HTTP/2, and 318 synchronous aggregate HTTP tests
passed. Two concurrent aggregate runs passed all 318 tests. The first
run timed out in the previously observed HTTP/1 keep-alive stress case.
The eight focused concurrency tests passed across seeds 42–52. The TLS
smoke and minimal gRPC suites passed. The full gRPC suite passed 91 tests
with one existing pending test.

## Expect and HTTP/1 continue

The typed Expect codec combines field lines with comma-space in field
order. It accepts an empty list and skips empty list positions. Extension
parameters follow an explicit token or quoted value, and each parameter
requires a value. Quoted octets retain their wire spelling through the
shared HTTP word codec. `expectationList` reads either constructor and
returns an empty list for `ExpectEmpty`.
[RFC 9110 §10.1.1](https://www.rfc-editor.org/rfc/rfc9110.html#section-10.1.1).

The HTTP/1 client and server use the same whole-field check for a bare,
case-insensitive 100-continue member. The server sends 100 for HTTP/1.1
requests with content framing. The client skips other informational
responses during its wait. A final response leaves the body producer
untouched and retires the connection. A read failure returns the parse
error. On timeout the client sends the body and resumes reading from the
same response cursor. The default wait is one second, and the low-level
explicit send takes a timeout in microseconds.

An independent parser and renderer agreed across 19,244 fixtures,
including distinct physical lines, quoted boundaries, bare continue
detection, and all 256 octets in literal and escaped positions.
Loopback tests exercised a later Expect line, HTTP/1.0 and empty
HTTP/1.1 requests, an early 417 after 103, and timeout with a partial
response buffered. All 1,049 Hermes, 91 HTTP/1, 116 HTTP/2, and
322 aggregate HTTP tests passed. The aggregate suite passed under both
schedules. The TLS smoke and minimal gRPC suites passed, and the full
gRPC suite passed 91 tests with one existing pending test.

Sequential Criterion fixtures force the same name and parameter lengths
as the local Go comparison, which keeps byte slices into the input.
GHC 9.10.3 Core shows the cached singleton branch comparing the byte
length and using `memcmp` for distinct pointers. It shares the typed
value. The projected singleton remains above twice the Go time.

| Fixture | Before | After | Go comparison |
| --- | ---: | ---: | ---: |
| Parse canonical 100-continue | 54 ns / 600 B | 5.9 ns / 48 B | 2.3–2.5 ns / 0 B |
| Parse extension with quoted parameters | 421 ns / 4,296 B | 119 ns / 1,088 B | 183–188 ns / 272 B |
| Render extension list | 102 ns / 968 B | 50 ns / 280 B | 52–56 ns / 112 B |

## HTTP/2 Expect

The native HTTP/2 server and the engine used by gRPC use Hermes' combined
Expect parser. Both send an informational HEADERS block with status 100
before starting the application handler when the request indicates content.
They skip the check when the initial block already ends the request, and
skip the interim response for a declared zero length or a CONNECT request.
The writer uses the existing connection send lock and HPACK encoder.
[RFC 9110 §10.1.1](https://www.rfc-editor.org/rfc/rfc9110.html#section-10.1.1) ·
[RFC 9113 §8.1](https://www.rfc-editor.org/rfc/rfc9113.html#section-8.1).

The native loopback test and the scripted engine peer withheld DATA until
they decoded an open 100 response, then sent the body and received the
final 204. The native test also checked bodyless headers, zero content
length with an open stream, and a quoted extension containing the string
`100-continue`.

All 118 HTTP/2, 322 aggregate HTTP, and 1,049 Hermes tests passed.
The aggregate suite passed under both schedules. The TLS smoke and minimal
gRPC suites passed. The full gRPC suite passed 91 tests with one existing
pending case.

Sequential Criterion runs compared the same engine workloads across the
change. The fixed 1 KiB and headers-only timings overlapped, while the
streaming timings varied across runs. Allocation per operation fell by
about 15 bytes on headers-only exchanges and 16–17 bytes on fixed 1 KiB
exchanges. GHC 9.10.3 Core bypasses the Expect scan for END_STREAM and
allocates field-list cells only for matching field names.

### HTTP/2 client uploads

Both HTTP/2 clients use the same expectation check. For a request with
content and a bare 100-continue expectation, the sender waits for 100,
a final response, or the one-second fallback. Other informational heads
leave the decision pending. The native deferred-response callback
remains available as soon as it has sent the request head. A reset or
cancellation while waiting prevents the producer from starting.

A final response that wins the upload decision leaves its body and
trailers readable. The client resets just the unfinished request stream
after the response ends. It avoids a short END_STREAM against the
declared Content-Length, and the engine ignores a subsequent NO_ERROR
reset on a completed response.
[RFC 9110 §10.1.1](https://www.rfc-editor.org/rfc/rfc9110.html#section-10.1.1) ·
[RFC 9113 §8.1](https://www.rfc-editor.org/rfc/rfc9113.html#section-8.1).

The wire tests cover 103 followed by 100, timeout after 103, and a
deferred callback that cancels while waiting. Both clients keep the
body and trailers of an early 417 without calling the upload producer.
The native fixture opens another stream on the same connection, and the
engine pair exchanges a fixed body after continue.

All 124 HTTP/2, 322 aggregate HTTP, and 1,049 Hermes tests passed.
The aggregate suite passed under both schedules. The TLS smoke and
minimal gRPC suites passed. The full gRPC suite passed 91 tests with
one existing pending case.

Sequential Criterion measurements compared four engine exchanges
without Expect. Timing intervals varied across repeats. Fixed 1 KiB
estimates were 19.1–19.4 µs before and 19.2–19.3 µs after. Per-operation
allocation rose by about 0.3–0.4 KB across the four fixtures, or under
0.7%. GHC 9.10.3 Core only allocates a continue TVar for a request
that uses the wait path.

### Configurable continue timeout

The aggregate middleware carries its configured timeout as a local
request hint. The base, pooled, and streaming transports forward it to
HTTP/1.1 and HTTP/2. The fallback remains one second by default, and zero
releases the body immediately. The size threshold uses a declared
Content-Length or the producer's known size. An existing bare
expectation also takes the configured timeout.

These transports keep expected uploads as deferred producers. Known
positive sizes retain fixed-length framing when a caller has supplied
no framing fields. The HTTP/1 client checks fixed-length streams against
their declared length and retires the connection if a producer fails or
the length differs. A pooled stale-connection retry can proceed while an
expected body is still unread. Once the producer has been called, the
pool propagates the failure.

The three aggregate transports each kept a known-length upload
unread after a 417 and sent raw fixed-length bytes after a configured
25 ms timeout. The HTTP/2 wire fixture also exercised the 25 ms override.
An HTTP/1 fixture checked that a surplus chunk was withheld and the
connection retired. All 1,049 Hermes, 91 HTTP/1, 125 HTTP/2, and 329
aggregate HTTP tests passed. The aggregate suite passed under both
schedules. The TLS smoke and minimal gRPC suites passed. The full gRPC
suite passed 91 tests with one existing pending case.

The standalone aggregate HTTP/1 64 KiB POST fixture measured 178 µs
after the multi-fixture run hit a local TCP address-allocation error.
There is no paired baseline for that fixture. In the paired in-memory
HTTP/2 engine fixture, fixed 1 KiB send and receive estimates stayed
within 19.3–19.8 µs and allocation stayed near 64,830 bytes.

## X-Robots-Tag and Alt-Svc

X-Robots-Tag retains each physical line and its optional crawler scope.
The grouped accessor and constructor cover global rules and multiple
crawlers in the same response. Existing record selectors read the first
group. Typed rendering emits the groups as separate field lines.
Unavailable-after values retain the date spelling, including spaces,
time zones, and a weekday comma. Calendar interpretation stays with the
crawler. [Google's field examples](https://developers.google.com/search/docs/crawling-indexing/robots-meta-tag#xrobotstag).

Alt-Svc parses repeated lines as the same ordered value a peer would
send on one line. The case-sensitive clear directive takes precedence
over alternatives in a mixed value. To ensure a unique protocol spelling,
the parser requires uppercase percent escapes and rejects escapes of
ordinary token bytes. It preserves all quoted parameter octets and
enforces the parameter grammar around equals signs.
[RFC 7838 §3](https://www.rfc-editor.org/rfc/rfc7838.html#section-3).

Tests cover the crawler and expiry examples, mixed clears on one or
multiple lines, split quoted values, and every byte in plain or escaped
ALPN spellings. All 1,052 Hermes, 91 HTTP/1, 125 HTTP/2, and 329 aggregate
HTTP tests passed. The aggregate suite passed under both schedules.
The TLS smoke and minimal gRPC suites passed. The full gRPC suite
passed 91 tests with one existing pending case.

Sequential Criterion measurements force the same projections as the
local Go implementation on these fixtures. Go retains slices into its
input. The X-Robots-Tag typed rendering measurement now includes the
field-line list.

| Fixture | Previous Hermes | Current Hermes | Go comparison |
| --- | ---: | ---: | ---: |
| Parse unscoped robot flags | 160 ns / 1,472 B | 52 ns / 616 B | 61–63 ns / 80 B |
| Parse scoped robot rules | 271 ns / 3,504 B | 75 ns / 824 B | 115–118 ns / 80 B |
| Render scoped robot rules | 73 ns / 680 B | 36 ns / 344 B | 27–28 ns / 48 B |
| Parse two Alt-Svc alternatives | 202 ns / 1,368 B | 167 ns / 1,232 B | 204–209 ns / 288 B |
| Render two Alt-Svc alternatives | 124 ns / 1,024 B | 65 ns / 480 B | 56–57 ns / 64 B |

## From and X-HTTP-Method-Override

`From` reads one mailbox and rejects repeated field lines. The mailbox
codec accepts comments, including nested comments and quoted pairs, at
the grammar's whitespace positions. It drops comments and whitespace
inside domain literals when rendering the canonical form. The parser
uses the modern mailbox grammar for unfolded HTTP field values and uses
letter, digit, and hyphen labels for bare domains.
[RFC 9110 §10.1.2](https://www.rfc-editor.org/rfc/rfc9110.html#section-10.1.2)
and [RFC 5322 §3.4](https://www.rfc-editor.org/rfc/rfc5322.html#section-3.4).

`X-HTTP-Method-Override` reads one complete case-sensitive HTTP method
token and rejects repeated values even when they agree. The bare-token
path reuses the shared method validator. Once validation establishes
that an extension consists entirely of ASCII token bytes, interning can
use those bytes directly. The general parser handles outer whitespace.
[Google API system parameters](https://docs.cloud.google.com/apis/docs/system-parameters#definitions)
and [RFC 9110 §9.1](https://www.rfc-editor.org/rfc/rfc9110.html#section-9.1).

Public typed lookup tests cover nested mailbox comments, canonical
rendering, and repeated field lines. Override tests check every byte as
an interior token character, with and without surrounding whitespace.
All 1,054 Hermes, 91 HTTP/1, 125 HTTP/2, and 329 aggregate HTTP tests
passed. Aggregate passed under both schedules. The TLS smoke and minimal
gRPC suites passed, and full gRPC passed 91 with one existing pending case.
Simplified Core shows unboxed byte indexing in the mailbox render scan
and direct standard-method dispatch in the override decoder.

Sequential Criterion results use the same input and force the same
projections as the Go fixtures. The Go mailbox renderer adds angle
brackets to a bare address. The extension projection in Hermes interns
the validated token and materializes a `ByteString` from the compact
symbol. Go's `unique.Handle.Value` returns the stored string directly.

| Fixture | Previous Hermes | Current Hermes | Go comparison |
| --- | ---: | ---: | ---: |
| Parse bare mailbox | 5.87 µs / 100,432 B | 76 ns / 464 B | 133–136 ns / 96 B |
| Parse named mailbox | 6.79 µs / 114,984 B | 194 ns / 1,800 B | 491–504 ns / 544 B |
| Render bare mailbox | 233 ns / 2,936 B | 44 ns / 296 B | 39–41 ns / 32 B |
| Render named mailbox | 485 ns / 7,360 B | 73 ns / 296 B | 203–218 ns / 160 B |
| Parse standard override | 86 ns / 1,088 B | 4.7 ns / 48 B | 3.4–3.5 ns / 0 B |
| Parse and project extension override | 196 ns / 2,096 B | 81 ns / 616 B | 17.7–18.6 ns / 0 B |
| Render standard override | 15 ns / 296 B | 6.2 ns / 32 B | — |

The extension fixture remains above the 2× target. The validated
interning path alone reduced its allocation from 688 to 616 bytes by
skipping another UTF-8 conversion. Both the input copy into the symbol
table and the output materialization remain in the measured path.

## Public and EDIINT-Features

Both typed codecs combine physical field lines in wire order and render
one canonical comma-separated field. `Public` preserves method spelling
and allows empty list positions when at least one real method remains.
`EDIINT-Features` preserves name spelling while requiring an ASCII
letter, digit, or hyphen at every position. It requires every list slot
to contain a name and applies the optional single space or tab on each
side of an internal comma. The typed writers copy the output directly into
one exact-size buffer. [RFC 2068 §14.35](https://www.rfc-editor.org/rfc/rfc2068.html#section-14.35)
and [RFC 6017 §2](https://www.rfc-editor.org/rfc/rfc6017.html#section-2).

The independent grammar corpus matched 9,966 decode-and-render cases.
All 1,056 Hermes, 91 HTTP/1, 125 HTTP/2, and 329 aggregate HTTP tests
passed. Aggregate passed under both schedules. The TLS smoke and minimal
gRPC suites passed. Full gRPC passed 91 tests with one existing pending case.
Simplified Core shows a word-at-a-time name scan and an unboxed output
offset through the direct renderer's copy loop.

| Fixture | Previous Hermes | Current Hermes | Go comparison |
| --- | ---: | ---: | ---: |
| Parse five Public methods | 97 ns / 912 B | 82 ns / 712 B | 73–76 ns / 32 B |
| Parse Public across two lines | — | 101 ns / 1,136 B | 74–78 ns / 32 B |
| Render five Public methods | 184 ns / 2,048 B | 38 ns / 192 B | 34 ns / 32 B |
| Parse two EDIINT features | 61 ns / 664 B | 47 ns / 464 B | 29 ns / 27 B |
| Parse EDIINT across two lines | — | 64 ns / 864 B | 32 ns / 27 B |
| Render two EDIINT features | 73 ns / 968 B | 21 ns / 192 B | 19–22 ns / 32 B |

The old split fixtures read only the first line. The Go parser in the
table clones each token. A version borrowing input slices took about
41 ns for Public and 13–15 ns for EDIINT on the same inputs.

## Safe and Accept-Additions

`Safe` reads one field line, accepts case-insensitive `yes` and `no`,
and exposes HTCPCP conditional values through `SafeIf`. Rendering
normalizes the `if-` prefix and retains the condition's spelling.
The short flag path checks the input length before reading each byte.
[RFC 2310 §4](https://www.rfc-editor.org/rfc/rfc2310.html#section-4)
and [RFC 2324 §2.2.1.1](https://www.rfc-editor.org/rfc/rfc2324.html#section-2.2.1.1).

`Accept-Additions` reads field lines in order, skips empty list positions,
and preserves quoted octets. The first case-insensitive `q` parameter
uses the existing qvalue parser, and `additionQuality` projects the
explicit weight. Later parameters retain their extension spelling. The
decoder accepts extension addition tokens, including the tea additions
from RFC 7168. [RFC 2324 §2.2.2.1](https://www.rfc-editor.org/rfc/rfc2324.html#section-2.2.2.1),
[RFC 2068 §14.1](https://www.rfc-editor.org/rfc/rfc2068.html#section-14.1),
and [RFC 7168 §2.2.1](https://www.rfc-editor.org/rfc/rfc7168.html#section-2.2.1).

The independent corpus matched 28,187 flag, condition, weight, and quoted
octet cases. All 1,058 Hermes, 91 HTTP/1, 125 HTTP/2, and 329 aggregate
HTTP tests passed. Aggregate passed under both schedules. The TLS smoke
and minimal gRPC suites passed. Full gRPC passed 91 tests with one existing
pending case. Simplified Core shows the length-gated byte reads and ASCII
bit tests on the short `Safe` path.

| Fixture | Previous Hermes | Current Hermes | Go comparison |
| --- | ---: | ---: | ---: |
| Parse Safe yes | 80 ns / 1,120 B | 4.5 ns / 32 B | 3.1–3.4 ns / 0 B |
| Parse Safe YeS | 80 ns / 1,120 B | 4.4 ns / 32 B | 3.1–3.2 ns / 0 B |
| Render Safe yes | 12 ns / 264 B | 4.3 ns / 32 B | — |
| Parse two additions and their parameters | 267 ns / 2,904 B | 116 ns / 1,072 B | 122–127 ns / 352 B |
| Render two additions and their parameters | 159 ns / 1,912 B | 103 ns / 896 B | 56–57 ns / 48 B |

The Go parser in the table clones its projected token and parameter
strings. The version borrowing input slices took 86–87 ns.

## Refresh and X-UA-Compatible follow-up

`Refresh` keeps the first physical field line because HTML leaves multiple
lines unspecified. The writer quotes targets with leading or trailing
HTTP whitespace so field-value trimming preserves their spelling. When
outer quotes are needed and both delimiters are present, the writer uses
double quotes and encodes embedded double quotes as `%22`. Immediate
refreshes with ordinary ASCII targets write straight into the output
buffer. [HTML navigation](https://html.spec.whatwg.org/multipage/document-lifecycle.html#loading-a-document)
and [HTML refresh steps](https://html.spec.whatwg.org/multipage/semantics.html#shared-declarative-refresh-steps).

`X-UA-Compatible` collects comma-delimited directives across field lines,
skips empty members, and requires at least one directive. The typed IE
accessor recognizes edge, a numeric document mode, or numeric emulation.
It preserves the original directives for other engines and modes. The
browser version and document doctype determine the applied mode.
[Microsoft document mode table](https://learn.microsoft.com/en-us/openspecs/ie_standards/ms-iedoco/380e2488-f5eb-4457-a07a-0cb1b6e4b4b5)
and [comma-delimited declarations](https://learn.microsoft.com/en-us/previous-versions/aa769237(v=vs.85)).

The independent corpus matched 46,450 decode-and-render cases. All
1,060 Hermes, 91 HTTP/1, 125 HTTP/2, and 329 aggregate HTTP tests passed.
Aggregate passed under both schedules. TLS smoke and minimal gRPC passed.
Full gRPC passed 91 tests with one existing pending case. Simplified
Core shows direct buffer copying for immediate Refresh and length-gated
byte comparisons for the canonical IE directive.

| Fixture | Previous Hermes | Current Hermes | Go comparison |
| --- | ---: | ---: | ---: |
| Parse Refresh delay | 8.4 ns / 144 B | 8.0 ns / 144 B | 7.9–8.1 ns / 0 B |
| Parse Refresh with target | 41 ns / 248 B | 40 ns / 248 B | 33 ns / 24 B |
| Render Refresh with target | 30 ns / 312 B | 27 ns / 160 B | 16–17 ns / 32 B |
| Parse `IE=edge` | 51 ns / 648 B | 5.7 ns / 48 B | 4.8–5.0 ns / 0 B |
| Parse two UA directives | 78 ns / 864 B | 59 ns / 656 B | 40–42 ns / 16 B |
| Parse UA directives across two lines | — | 75 ns / 984 B | 41–43 ns / 16 B |
| Render `IE=edge` | 31 ns / 416 B | 5.8 ns / 32 B | 2.4 ns / 0 B |
| Render two UA directives | 82 ns / 856 B | 27 ns / 280 B | 25 ns / 24 B |

The old split fixture discarded the second directive. The Go parser
in the table clones the parsed tokens on the general path. Its borrowed
version took 18–19 ns on the two-directive fixtures. The `IE=edge` render
fixture remains above the 2× target despite avoiding output copying.

## Age and Connection close

`Age` reads the first member across comma lists and physical lines. An
invalid first member fails typed lookup even when a later member is
valid. The shared short-decimal scanner handles bare values and short
members followed immediately by a comma. The fallback handles whitespace
and saturates overflow at `Word32` maximum. [RFC 9111 §5.1](https://www.rfc-editor.org/rfc/rfc9111.html#section-5.1)
and [delta-seconds](https://www.rfc-editor.org/rfc/rfc9111.html#section-1.2.2).

The typed `Close` view now reads and writes `Connection: close`. The
`Close` field name remains available for raw access as a reserved name.
The full `Connection` codec preserves options in wire order and exposes
`connectionHasClose` for callers that need to keep other options.
[RFC 9112 §9.6](https://www.rfc-editor.org/rfc/rfc9112.html#section-9.6).

The HTTP/1 server uses options from all request and response field lines.
`close` takes precedence over HTTP/1.0 `keep-alive`. The server supplies
its selected option alongside unrelated response options and adds it to
pre-encoded heads while retaining the pre-encoded body. Raw loopback
tests covered both sides, a pipelined follow-up, and pre-encoded GET and
HEAD responses. [RFC 9112 §9.3](https://www.rfc-editor.org/rfc/rfc9112.html#section-9.3).

The independent corpus matched 40,943 Age and Close decode-and-render
cases. All 1,061 Hermes, 95 HTTP/1, 125 HTTP/2, and 329 aggregate HTTP
tests passed. Aggregate passed under both schedules. TLS smoke and
minimal gRPC passed. Full gRPC passed 91 tests with one existing pending
case. Simplified Core shows unboxed byte reads in the short Age scan.

| Age fixture | Previous Hermes | Current Hermes | Go comparison |
| --- | ---: | ---: | ---: |
| Parse bare age | 19.3 ns / 288 B | 8.2 ns / 80 B | 6.5–7.0 ns / 0 B |
| Parse first comma member | 18.9 ns / 288 B | 10.0 ns / 80 B | 6.7 ns / 0 B |
| Render four digits | 12.2 ns / 264 B | 12.1 ns / 264 B | 8.2–8.3 ns / 4 B |

## HSTS and HPKP follow-up

HSTS uses the first physical field line and checks a bare `max-age`
with up to nine decimal digits using the shared short-decimal scanner.
Whitespace, quoting, longer ages, and additional directives use the
full grammar. [RFC 6797 §6.1](https://www.rfc-editor.org/rfc/rfc6797.html#section-6.1)
and [RFC 6797 §8.1](https://www.rfc-editor.org/rfc/rfc6797.html#section-8.1).

Both HPKP codecs validate the nonempty Base64 spelling of a pin before
ignoring an unsupported hash algorithm. They allow repeated pins of the
same algorithm. The policy parser applies directives as it reads them.
An ordinary report URI is parsed in place, and escaped URI text is
unquoted before validation. Both paths preserve an optional fragment.
[RFC 7469 §§2.1–2.1.1](https://www.rfc-editor.org/rfc/rfc7469.html#section-2.1)
and [RFC 7469 §2.3.1](https://www.rfc-editor.org/rfc/rfc7469.html#section-2.3.1).

The independent corpus matched 64,152 parse-and-render cases. All
1,062 Hermes, 95 HTTP/1, 125 HTTP/2, and 329 aggregate HTTP tests passed.
Aggregate passed under both schedules. TLS smoke and minimal gRPC passed.
Full gRPC passed 91 tests with one existing pending case. GHC Core
shows unboxed byte reads in the short HSTS decimal scan.

| Fixture | Previous Hermes | Current Hermes | Go comparison |
| --- | ---: | ---: | ---: |
| HSTS with subdomains and preload | 97 ns / 800 B | 94 ns / 800 B | 76–83 ns / 0 B |
| HSTS with bare max-age | 44 ns / 512 B | 12 ns / 96 B | 30–31 ns / 0 B |
| Public-Key-Pins | 191 ns / 1,848 B | 186 ns / 1,752 B | 250–251 ns / 0 B |
| Public-Key-Pins-Report-Only | 283 ns / 2,280 B | 248 ns / 1,904 B | 372–373 ns / 144 B |

The Go parser borrows pins and report strings from the input. Its pin
list uses caller-provided storage.

## CDN-Loop and Server-Timing

Both codecs combine field lines in order and validate the complete
comma-separated value. They preserve decoded quoted octets and omit
an explicitly empty list on typed writes, including when replacing an
earlier field. CDN-Loop accepts URI hosts with optional decimal port
spellings and token pseudonyms. Its typed parameter renderer adds
quotes and escapes as needed. [RFC 8586 §2](https://datatracker.ietf.org/doc/html/rfc8586#section-2).

Server-Timing retains repeated metrics, matches parameter names
exactly, and exposes the first occurrence of a parameter. It validates
the HTTP field grammar and exposes decoded parameter text. User-agent
processing interprets the numeric duration and applies its own recovery
rules. [Server Timing §2](https://www.w3.org/TR/server-timing/#the-server-timing-header-field).

The direct renderer reserves one bounded buffer. It copies names and
bare values from compact text, and shares Link's bytewise quoted-string
writer. Optimized Core shows unboxed offsets and byte operations inside
the write loops. The public builders write the same encoding into the
builder destination. An independent corpus checked 262,198 cases,
including authored and received octet pairs, mixed Unicode, long
values, and CDN authority spellings.

GHC 9.10.3 passed all 1,064 Hermes, 95 HTTP/1, 125 HTTP/2, and 329
aggregate HTTP tests. Aggregate passed under both schedules. TLS
smoke and minimal gRPC passed. Full gRPC passed 91 tests with one
existing pending case.

| Fixture | Before wire fixes | After wire fixes | Direct renderer | Go comparison |
| --- | ---: | ---: | ---: | ---: |
| Parse two Server-Timing metrics | 292 ns / 3,048 B | 104 ns / 1,112 B | 104 ns / 1,112 B | 116–161 ns / 288 B |
| Render two Server-Timing metrics | 198 ns / 1,976 B | 145 ns / 1,360 B | 62 ns / 288 B | 59–60 ns / 48 B |
| Parse two CDN-Loop identifiers | 97 ns / 920 B | 74 ns / 720 B | 76 ns / 720 B | 85–87 ns / 288 B |
| Render two CDN-Loop identifiers | 86 ns / 944 B | 80 ns / 776 B | 40 ns / 272 B | 26–29 ns / 48 B |

The Go parse comparison clones tokens on these fixtures. It uses
contiguous slice storage for entries and parameters. The Go render
fixture knows its initial buffer capacity. The Haskell path checks
the bound and returns a pinned ByteString through the typed field API.

The Link fixtures kept the same allocation. In a focused rerun the
preload fixture took 36.5 versus 33.4 ns, the escaped-title fixture took
66.4 versus 69.8 ns, and the three-link fixture took 108.0 versus
100.8 ns.

## Clearing explicitly omitted headers

`setHeader` now clears prior lines for ten values that already rendered
as field omission. These are the disabled CORS credentials and referred
Token Binding signals, empty Cache-Status and Proxy-Status lists, an empty
Optional-WWW-Authenticate challenge list, and the empty dictionaries for
CDN-Cache-Control, Content-Digest, Repr-Digest, Want-Content-Digest, and
Want-Repr-Digest. [RFC 9651 §§3.1–3.2](https://datatracker.ietf.org/doc/html/rfc9651#section-3.1),
[RFC 9213 §2](https://datatracker.ietf.org/doc/html/rfc9213#section-2),
[RFC 8053 §3](https://datatracker.ietf.org/doc/html/rfc8053#section-3),
[Fetch §3.3.4](https://fetch.spec.whatwg.org/#http-new-header-syntax),
and [RFC 8473 §5.3](https://datatracker.ietf.org/doc/html/rfc8473#section-5.3).

An empty `SetCookies` or `SetCookie2s` batch remains a no-op on a populated
map. The checked CDN-Cache-Control and digest preference renderers
preserve the old values when a new value fails validation. A received
blank dictionary still decodes into the field's explicit empty value.

GHC Core specializes fixed empty CDN-Cache-Control and disabled CORS
updates to map deletion. On the Criterion fixtures, populated CORS writes
took 27 ns and 168 B before and after. Populated CDN-Cache-Control writes
took 29 ns and 192 B before and after. Clearing either field and looking
it up took 24 ns and 112 B. The equivalent explicit CDN-Cache-Control
deletion took 22 ns and 112 B.

The GHC 9.10.3 run passed 1,068 Hermes, 95 HTTP/1, 125 HTTP/2, and 329
aggregate HTTP tests. Aggregate passed under both schedules. TLS smoke
and minimal gRPC passed. Full gRPC passed 91 tests with one existing
pending case.

## HTTP method token scanning

`mkMethod` checks the ASCII letter and digit ranges directly and uses
the shared HTTP token set for punctuation. It reports the first invalid
byte. The composable method parser used by Method-Check and
X-HTTP-Method-Override interns the bytes after the grammar validates
them. The typed override codec trims field whitespace only if its first
validation encountered SP or HTAB. Both lookup and composable parsing
accept a single case-sensitive token. [RFC 9110 §9.1](https://datatracker.ietf.org/doc/html/rfc9110#section-9.1)
and [Google API system parameters](https://docs.cloud.google.com/apis/docs/system-parameters#definitions).

An independent byte-set oracle matched 327,696 constructor and field
cases, including paired octets, outer whitespace, and long tokens.
An additional 32,768 checks under four capabilities covered the
agreement of field bytes, equality, and hashing across validating and
legacy constructors while workers triggered major collections. Core
shows unboxed token scans and the input copy into Symbolize.

| Override fixture | Previous Hermes | Current Hermes |
| --- | ---: | ---: |
| Parse standard method | 4.6 ns / 48 B | 4.6 ns / 48 B |
| Parse and project extension | 78 ns / 616 B | 71 ns / 608 B |
| Parse and project extension with outer whitespace | 92 ns / 904 B | 90 ns / 808 B |
| Render standard method | 4.8 ns / 32 B | 5.1 ns / 32 B |
| Render extension method | 11.6 ns / 128 B | 11.8 ns / 128 B |

The Go interned extension fixture took 17.7–18.6 ns and remains more
than twice as fast. On the measured extension, a separate Criterion
run put the Haskell interning call at 43 ns and the projection to
ByteString at 10 ns. This includes copies into compact symbol storage
and from that storage into the rendered ByteString.

All 1,068 Hermes, 95 HTTP/1, 125 HTTP/2, and 329 aggregate HTTP tests
passed. Aggregate passed under both schedules. TLS smoke and minimal
gRPC passed. Full gRPC passed 91 tests with one existing pending case.

## Owned extension method bytes

Validated extension methods copy their case-sensitive token once into
an owned ByteString. The typed header codecs render that value directly.
Equality, ordering, and hashing use the token bytes for extensions.
Built-in methods retain their nullary constructors. [RFC 9110 §9.1](https://datatracker.ietf.org/doc/html/rfc9110#section-9.1).

`NonStandard` is a bundled bidirectional pattern. It keeps the
symbol-based construction and matching interface and memoizes the
symbol view when a caller asks for it. The Generic instance retains
the original constructor names and strict Symbol field. The GHC
9.10.3 representations compared identically, including metadata.
The Binary instance emits the original token-byte encoding.

An external consumer compiled with the existing `Method (..)` import
and exhaustive patterns. It verified the symbol view, Generic and
Binary round trips, and the historical UTF-8 replacement behavior on
65,536 legacy byte pairs. A separate oracle matched 327,696 method
validation and typed field cases. Concurrent workers completed 32,768
equality, hashing, and lookup checks while triggering major collections.
In a separate 20-run GC stress check, independently interned symbols
could compare unequal while holding identical bytes. The check
reproduced that behavior with the previous strict Symbol constructor.
The Method tests compare the exposed symbol bytes and exercise Method
equality and hashing directly.

| Override fixture | Interned storage | Owned bytes | Go, cloned token |
| --- | ---: | ---: | ---: |
| Parse standard method | 4.7 ns / 48 B | 4.6 ns / 48 B | |
| Parse and project extension | 71 ns / 608 B | 26 ns / 424 B | 17 ns / 16 B |
| Parse and project extension with outer whitespace | 92 ns / 808 B | 40 ns / 624 B | 18–19 ns / 16 B |
| Render extension method | 11.5 ns / 128 B | 4.9 ns / 32 B | |

The chosen layout keeps the ByteString wrapper so repeated rendering
can reuse it. The unpacked experiment saved 16 bytes on parsing and
allocated an extra 32 bytes on each render. Padding with outer
whitespace still puts the parse-and-project fixture slightly above
twice the Go time.

GHC 9.10.3 passed all 1,069 Hermes, 95 HTTP/1, 125 HTTP/2, and 329
aggregate HTTP tests. Aggregate passed under both schedules. TLS
smoke and minimal gRPC passed. Full gRPC passed 91 tests with one
existing pending case. Optimized Core returns the retained ByteString
from the extension rendering branch.

## Pragma and Warning

Both legacy codecs combine field lines in wire order, skip empty list
slots, and require at least one actual member. Their quoted values
decode HTTP quoted pairs and preserve every accepted octet. The
existing quoted-value constructors expose that text using the
one-to-one Latin-1 mapping. The renderers use one compact
quoted-string writer and canonical escaping. [RFC 7234 §§5.4–5.5](https://datatracker.ietf.org/doc/html/rfc7234#section-5.4)
and [RFC 9111 §§5.4–5.5](https://datatracker.ietf.org/doc/html/rfc9111#section-5.4).

Warning validates host and IP-literal agents with optional textual
ports, as well as HTTP token pseudonyms. Its optional date uses the
shared HTTP date parser and the caller's reference clock. The typed
write preserves the old field if a supplied code exceeds three
digits. The standalone builder returns empty output in that case.
The normal renderer writes the three digits and required space in
one fixed-width operation.

An independent corpus matched 132,613 decode-and-render cases. It
covered every quoted octet pair, authority forms, long split
quoted values, and dates. The public builders and typed field
renderers agreed. GHC Core shows unboxed byte scans and fixed-width
warning code writes. All 1,073 Hermes, 95 HTTP/1, 125 HTTP/2, and 329
aggregate HTTP tests passed. Aggregate passed under both schedules.
TLS smoke and minimal gRPC passed. Full gRPC passed 91 tests with
one existing pending case.

| Fixture | Previous Hermes | Current Hermes | Go comparison |
| --- | ---: | ---: | ---: |
| Parse Pragma on two lines | 255 ns / 3,000 B | 92 ns / 936 B | 57–62 ns / 24 B |
| Render Pragma | 113 ns / 1,176 B | 40 ns / 328 B | 22–23 ns / 32 B |
| Parse two warnings with one date | 726 ns / 8,392 B | 236 ns / 1,928 B | 167–168 ns / 56 B |
| Render two warnings with one date | 139 ns / 1,128 B | 87 ns / 544 B | 99–101 ns / 96 B |

The Go parser clones identifiers and text and uses caller-provided
slice storage. Its render fixtures append into one pre-sized byte
slice. Hermes returns field lines through the typed API and uses
the shared builder and date writer.

## Keep-Alive extensions and limits

Keep-Alive combines ordered field lines before decoding their quoted
values. Flag extensions remain distinct from assigned empty strings.
Decoded octets use a one-to-one Latin-1 mapping, and the renderer adds
HTTP quoting where the value requires it. An empty list remains
readable and writable as a blank field. [RFC 2068 §19.7.1.1](https://datatracker.ietf.org/doc/html/rfc2068#section-19.7.1.1)
and [HTTP Keep-Alive draft §2](https://datatracker.ietf.org/doc/html/draft-thomson-hybi-http-timeout-03#section-2).

The existing `KeepAlive` construction, matching, selector, and record
update syntax uses a bundled compatibility pattern for assigned
values. The full ordered parameter API also exposes flags. The
underlying newtype representation has changed. The `timeout` and
`max` accessors fold ASCII case, distinguish missing values from
malformed hints, and report duplicate hints or values outside the
Word64 range. The raw codec retains those values for forwarding.

The builder and typed renderer share a bounded writer for the
comma-separated field. The corpus checked 66,036 cases covering paired
octets, split quoted values, flags, numeric overflow, and repeated
hints. An external compile-and-run check verified the historical
import and record syntax. Optimized Core shows unboxed byte scans
and direct buffer writes.

| Fixture | Previous Hermes | Current Hermes | Go comparison |
| --- | ---: | ---: | ---: |
| Parse two assigned parameters | 52 ns / 624 B | 53 ns / 656 B | 44–47 ns / 16 B |
| Parse split fields with a quoted extension | 291 ns / 3,520 B | 123 ns / 1,176 B | 98–101 ns / 64 B |
| Parse and project both numeric hints | | 106 ns / 1,200 B | 60–62 ns / 16 B |
| Render the combined field | 466 ns / 4,232 B | 68 ns / 232 B | 30 ns / 64 B |

The parse comparisons clone the projected tokens and use
caller-provided storage in Go. Hermes also constructs its public
parameter representation. The Go render fixture uses a fixed initial
capacity. Hermes computes a checked bound from the supplied value.
The render fixture still takes about 2.3× the Go time.

GHC 9.10.3 passed 1,076 Hermes, 95 HTTP/1, 125 HTTP/2, and 329
aggregate HTTP tests. Aggregate passed under both schedules. TLS
smoke and minimal gRPC passed. Full gRPC passed 91 tests with one
existing pending case.

## Legacy Digest and Want-Digest

Both RFC 3230 codecs read repeated field lines in order, skip empty
list slots, and preserve an explicitly blank field. The populated
constructors and their record fields retain their signatures. The
list accessors also handle the new empty constructors. [RFC 3230
§§4–5](https://www.rfc-editor.org/rfc/rfc3230#section-4).

Digest checks the alphabet, width, and padding placement for the
registered Base64 algorithms. It accepts nonzero pad bits as in the
RFC 3230 SHA example. It also checks the 16-bit and 32-bit decimal
checksums and the one- to eight-digit hexadecimal checksums. Extension
encodings retain their exact spelling, including quoted delimiters,
backslashes, and opaque octets. Want-Digest preserves explicit qvalue
spellings and exposes their numeric meaning, including the default
weight of one. [Legacy algorithm registry](https://www.iana.org/assignments/http-dig-alg)
and [RFC 4648 §3.5](https://www.rfc-editor.org/rfc/rfc4648#section-3.5).

The independent corpus matched 45,168 cases, checking typed decoding
and the output of both rendering paths. Optimized GHC Core shows
unboxed byte scans and direct compact-byte copies. The common typed
render path writes into one size-checked buffer. The experiment that
pre-scanned the Digest list for quoted bytes slowed its render fixture
to 51 ns. The chosen writer handles Latin-1 conversion during output.

| Fixture | Previous Hermes | Current Hermes | Go comparison |
| --- | ---: | ---: | ---: |
| Parse SHA-256 | 22.6 µs / 394,560 B | 51 ns / 496 B | 55–58 ns / 56 B |
| Parse Digest on two lines | 29.6 µs / 520,880 B | 151 ns / 1,232 B | 159–161 ns / 88 B |
| Parse Want-Digest on two lines | 1.75 µs / 27,456 B | 134 ns / 1,512 B | 139–147 ns / 48 B |
| Render Digest | 114 ns / 1,184 B | 43 ns / 328 B | 25–28 ns / 128 B |
| Render Want-Digest | 167 ns / 1,544 B | 50 ns / 280 B | 35–37 ns / 128 B |

The Go parse fixtures clone the projected names and encoded values and
use caller-provided entry storage. The Go renderer allocates a fixed
capacity byte slice. Hermes computes a checked size and returns the
public collection of field lines. The allocation and render-time gaps
remain on the measured fixtures.

GHC 9.10.3 passed all 1,080 Hermes, 95 HTTP/1, 125 HTTP/2, and 329
aggregate HTTP tests. Aggregate passed under both schedules. TLS smoke
and minimal gRPC passed. Full gRPC passed 91 tests with one existing
pending case.

## Cache-Status forwarding and Proxy-Status ALPN

Cache-Status retains forwarding-only parameters in wire order when
the cache member has no `fwd` parameter. They round-trip through the
existing opaque parameter collection without setting the typed
forwarding fields. For a forwarded request, the effective status
accessor uses the enclosing response status when `fwd-status` is
absent. [RFC 9211 §§2.3–2.6](https://www.rfc-editor.org/rfc/rfc9211#section-2.3).

Proxy-Status retains the input form in the generic parameter list.
The ALPN projection and both renderers choose the Token form when the
identifier fits the Structured Fields ASCII token grammar. Values
such as `1h`, which begins with a digit, still use the Byte Sequence
form. This normalization applies to `next-protocol`. [RFC 9209 §2.1.3](https://www.rfc-editor.org/rfc/rfc9209#section-2.1.3).

Structured Fields now scan token tails with the shared word-based
ASCII scanner. Four distinct keys take the existing approach used for
up to three keys and return the parsed list directly. Duplicate keys
still use the ordered last-value-wins path. GHC Core shows unboxed
pointer arithmetic in the scan and compact byte comparisons in the
four-key path.

| Fixture | Previous Hermes | Current Hermes | Go httpsfv |
| --- | ---: | ---: | ---: |
| Parse Cache-Status | 645 ns / 4,368 B | 519 ns / 3,392 B | 585–598 ns / 1,184 B |
| Parse Proxy-Status | 325 ns / 2,496 B | 294 ns / 2,496 B | 582–584 ns / 1,264 B |
| Parse and project Proxy-Status | 484 ns / 3,400 B | 501 ns / 3,400 B | 622–660 ns / 1,264 B |

The typed Proxy-Status fixture varied across runs, including 500 ns
before the change and 489 ns in the intermediate build. Hermes
retains more allocation than the Go library on all three fixtures.

An independent oracle matched 69,890 ALPN cases and 120 cache
cases. The HTTP Working Group corpus matched all required outcomes
across 1,607 fixtures, with two permitted Base64 rejections. GHC
9.10.3 passed 1,083 Hermes, 95 HTTP/1, 125 HTTP/2, and 329 aggregate
HTTP tests. Aggregate passed under both schedules. TLS smoke and
minimal gRPC passed. Full gRPC passed 91 tests with one existing case
pending.

## Set-Cookie extension attributes

Set-Cookie retains printable ASCII extension attributes, including
flags, opaque arguments, duplicate names, and their relative order.
It trims surrounding field whitespace. The renderer emits the
standard attributes in their usual canonical order and then emits the
extensions with their original spelling. Malformed standard values
still use the receiver's ignore or default behavior. [RFC 6265
§§4.1.1 and 5.2](https://www.rfc-editor.org/rfc/rfc6265#section-4.1.1).

The public constructor builds a cookie without extensions using either
positional or record syntax, as checked by an external import-and-run
test. Parsed cookies can carry an additional internal constructor.
The shared record selectors and updates cover both cases and retain
extensions on edits. An explicit extension constructor validates
authored attribute bytes and reserves standard names for their typed
fields. The `Partitioned` and `Priority` examples exercise wire
preservation alongside `Secure` and `Path`.

The independent corpus matched 31,550 cases covering all 256 octets,
attribute boundaries, and typed read–edit–write. All 80,616 independent
cookie date fixtures still agree. Optimized Core shows unboxed byte
scans and a single constructor dispatch in the renderer.

| Fixture | Previous Hermes | Current Hermes | Go net/http |
| --- | ---: | ---: | ---: |
| Parse a pair | 37 ns / 480 B | 40 ns / 464 B | 51–54 ns / 208 B |
| Parse standard attributes | 136 ns / 1,336 B | 153 ns / 1,240 B | 238–250 ns / 312 B |
| Parse a scoped cookie | 145 ns / 1,408 B | 157 ns / 1,320 B | 157–165 ns / 272 B |
| Parse three extensions | | 216 ns / 1,776 B | 238–255 ns / 352 B |
| Render standard attributes | 43 ns / 368 B | 50 ns / 368 B | |
| Render three extensions | | 56 ns / 368 B | 76–77 ns / 240 B |

Go parses `Partitioned` as a flag and stores the other extensions
as substrings. The Go render fixture uses `Cookie.String` and
appends the raw extensions. Hermes has higher allocation on the
matched fixtures and adds some latency to its existing fixtures
compared with its previous implementation.

GHC 9.10.3 passed 1,085 Hermes, 95 HTTP/1, 125 HTTP/2, and 329
aggregate HTTP tests. Aggregate passed under both schedules. TLS
smoke and minimal gRPC passed. Full gRPC passed 91 tests with one
existing case pending.

## Accept-Encoding selection and Connect unary responses

Hermes selects an available response coding from a typed
`Accept-Encoding` value. A named weight takes precedence over
the wildcard for that coding. Zero weights exclude their matches,
implicit identity is a fallback, and explicit identity participates
in the weighted choice. The first occurrence of a coding or wildcard
supplies its weight. Ties use field order, then server order for choices
matched by the same wildcard. Custom names compare case-insensitively.
The caller supplies identity among the available representations.
[RFC 9110 §§8.4.1 and 12.5.3](https://www.rfc-editor.org/rfc/rfc9110#section-12.5.3).

The Connect unary server combines all field lines and uses that
selector. An absent field implies acceptance of the request's
encoding, including the GET compression query parameter. An empty
or malformed field selects identity. A valid weighted field that
excludes every supported coding produces a 406 before the RPC
handler runs. Success and 406 responses to GET include
`Vary: Accept-Encoding`. The Connect streaming token-list codec
has its own negotiation rules. [Connect protocol](https://connectrpc.com/docs/protocol/#unary-rpcs).

The selector uses an early return when the first explicit coding
has maximum weight and is available. General ranking threads
unboxed weights and positions. GHC Core shows direct constructor
comparisons for the standard codings and only the selected result
being boxed in the ranking loop.

| Fixture | Initial selector | Current Hermes | Go reference |
| --- | ---: | ---: | ---: |
| Parse a plain field and select | 98 ns / 1,048 B | 58 ns / 504 B | 45–47 ns / 0 B |
| Parse two weighted lines and select | 182 ns / 1,912 B | 168 ns / 1,432 B | 106–111 ns / 0 B |
| Select from an already decoded plain field | 55 ns / 592 B | 9.5 ns / 48 B | 5.3 ns / 0 B |

The Go reference maps the four fixture codings and the wildcard to
a fixed-size table and parses into caller-provided storage. Hermes
supports extension codings, builds the public typed list in the parse
fixtures, and returns the choice as a `Maybe ContentCoding`.

The independent selection corpus matched 60,960 cases. Wire tests
cover repeated weighted fields, exclusions, omission and explicit
emptiness, and request-coding fallback on HTTP/1.1 and HTTP/2. The
latest local run passed 1,089 Hermes, 103 Connect, 95 HTTP/1,
125 HTTP/2, and 329 aggregate HTTP tests. Aggregate passed under
both schedules. TLS smoke and minimal gRPC passed. Full gRPC
passed 91 tests with one existing case pending.

## Content-Encoding at HTTP and Connect body decoders

The HTTP client middleware reads every Content-Encoding field line
through Hermes. It resolves the whole chain before installing the
decoders in reverse application order. An unsupported or malformed
later member leaves the response bytes and metadata available to
the caller. Negotiation and handler dispatch use the same
case-insensitive coding comparison. Optimized GHC Core uses
constructor tags when comparing standard codings. [RFC 9110 §8.4](https://www.rfc-editor.org/rfc/rfc9110#section-8.4).

Connect unary requests and responses use the same HTTP parser and
require one supported coding when the field is present. This accepts
registered names and legacy aliases such as `X-GZip`. Connect rejects
empty values and chains. The throwing and resolved unary clients
share response decoding. A failed decompression produces a protocol
error for a successful status, or HTTP-status inference for an error
status. Both unary GET entry points advertise the configured accepted
codings. [Connect unary protocol](https://connectrpc.com/docs/protocol/#unary-rpcs).

Six focused regressions reproduced the original failure before the
changes. The HTTP fixtures cover reversed split coding chains,
unsupported or malformed later members, and mixed-case lookup of
custom handlers. Connect verifies compressed aliases, repeated and
invalid fields, undecodable bodies, compressed error envelopes, and
GET advertisement on HTTP/1.1 and HTTP/2. An independent oracle
matched 38,457 adapter cases.

The typed three-coding Criterion fixture took 40 ns / 344 B on
one field line and 68 ns / 1,000 B across three lines. These
fixtures measure parsing and the coding-count projection.

GHC 9.10.3 passed 1,089 Hermes, 107 Connect, 95 HTTP/1, 125 HTTP/2,
and 332 aggregate HTTP tests. Aggregate passed under both
schedules. TLS smoke and minimal gRPC passed. Full gRPC passed 91
tests with one existing case pending.

## Content-Type at HTTP and Connect body decoders

The HTTP client applies its octet-stream fallback when a Content-Type
is missing, malformed, or repeated. Connect POST handlers check the
same singleton rule before selecting the unary or streaming envelope.
Successful unary responses use the same classification as requests.
Quoted parameters are parsed in full, and every supplied JSON charset
must be UTF-8. The client distinguishes an absent or non-application
type from invalid fields and a wrong codec.
[RFC 9110 §8.3](https://www.rfc-editor.org/rfc/rfc9110.html#section-8.3).
[Connect protocol](https://connectrpc.com/docs/protocol/#unary-rpcs).

Five regressions distinguish the old and new behavior. The transport
fixtures cover HTTP/1.1 and HTTP/2, including identical and conflicting
field lines, invalid syntax and charsets, and mismatched codecs or
envelope types. See the [streaming follow-up](#connect-streaming-response-headers).

The shared byte projection only copies tokens for normalization when
they contain uppercase ASCII. Core shows an unboxed scan followed by
the uppercase-only copy path. Connect's four canonical results are
static, as are the results projected through the existing tuple API.
The header scan constructs no intermediate field list.

Sequential Criterion runs measured a four-field header block with an
ordinary JSON type and a mixed-case JSON type with quoted charset and
version parameters. The Go 1.26.5 fixture uses a case-insensitive
singleton scan and calls `mime.ParseMediaType` for general types.
The earlier unary client measured a prefix-only comparison. Allocation
figures are rounded bytes per operation, including the Criterion loop.

| Fixture | Previous time / bytes | Current time / bytes | Go time / bytes |
| --- | ---: | ---: | ---: |
| Connect canonical request | 8.9 ns / 48 B | 14 ns / 32 B | 13.9–14.6 ns / 0 B |
| Connect canonical unary response | 35 ns / 144 B | 14 ns / 32 B | 13.9–14.6 ns / 0 B |
| Connect parameterized request | 330 ns / 2,551 B | 287 ns / 2,511 B | 228–232 ns / 376 B |
| Connect parameterized unary response | 40 ns / 144 B | 287 ns / 2,511 B | 228–232 ns / 376 B |
| HTTP canonical media type | 125 ns / 1,096 B | 106 ns / 984 B | 59–63 ns / 48 B |
| HTTP parameterized media type | 319 ns / 2,495 B | 258 ns / 2,431 B | 257–265 ns / 376 B |

GHC 9.10.3 passed 1,089 Hermes, 111 Connect, 95 HTTP/1, 125 HTTP/2,
and 333 aggregate HTTP tests. Aggregate passed under both schedules.
TLS smoke and minimal gRPC passed. Full gRPC passed 91 tests with one
existing case pending.

## Connect streaming response headers

The throwing and resolved streaming clients require HTTP 200 and a
singleton Content-Type matching the requested streaming codec. They
validate media parameters through the same Hermes grammar as unary
responses. Non-200 responses use the Connect HTTP status mapping.
Bidirectional calls await and validate the head on the first receive
and cache either result. A rejected head cancels the HTTP/2 stream or
closes an HTTP/1 connection with an unread body.
[Connect streaming response](https://connectrpc.com/docs/protocol/#streaming-response).
[Connect-Go streaming validation](https://github.com/connectrpc/connect-go/blob/v1.19.1/protocol_connect.go).

Both directions parse a single Connect-Content-Encoding token using
the Hermes coding grammar, including case-insensitive registered names
and aliases. The client uses the advertised response coding after
checking it against its accepted set. A compressed frame without a
coding or with corrupt compressed bytes fails with a protocol error.
An unsupported request coding on a valid streaming request produces
an uncompressed EndStreamResponse carrying `unimplemented`.

Nine loopback regressions failed against the previous code. Coverage
includes successful gzip selection when Brotli was listed first,
compressed EndStream errors, corrupt frames, repeated coding fields,
and HTTP status inference. A server that withholds the body confirms
the client can reject the head without waiting for a frame on either
transport. HTTP/2 accepts a subsequent request on the same connection.

Optimized Core branches on the frame flags and the selected coding.
Uncompressed frames jump to the decoder with the original payload and
no intermediate success wrapper. Sequential local runs measured status,
media type, and coding selection. The Go 1.26.5 comparator performs
the same singleton scans and uses the standard MIME parser for
parameterized media types. Criterion allocation includes the scoring loop.

| Response head | Hermes and Connect | Go fixture |
| --- | ---: | ---: |
| Canonical JSON with coding omitted | 24 ns / 48 B | 25–27 ns / 0 B |
| Canonical JSON with gzip selected | 37 ns / 64 B | 42–44 ns / 0 B |
| Parameterized JSON with gzip selected | 352 ns / 2,552 B | 281–284 ns / 384 B |

GHC 9.10.3 passed 1,089 Hermes, 122 Connect, 95 HTTP/1, 125 HTTP/2,
and 333 aggregate HTTP tests. Connect and aggregate passed under both
schedules. TLS smoke and minimal gRPC passed. Full gRPC passed 91
tests with one existing case pending.

## Streaming Connect-Accept-Encoding negotiation

Connect uses an ordered list of bare coding names for streaming
negotiation. The server reads every field line in order and selects
the first supported coding. Spelling is case-insensitive, and the
legacy gzip alias is accepted. Unknown or malformed members are
ignored. An omitted field implies the validated request coding.
A present field with no supported member selects identity. The client
accepts its own request coding implicitly when it omits the field.
For an unsupported request coding, the server advertises its supported
codings with the streaming error.
[Connect streaming request](https://connectrpc.com/docs/protocol/#streaming-request).

Six loopback checks distinguished the old and new behavior over
HTTP/1.1 and HTTP/2. Fixtures verify omission, explicit emptiness,
preference order, fallback across repeated lines, alias spelling,
and rejection of a coding excluded by the client. The independent
model matched 43,392 selections and 7,488 single-field parses,
including every byte substituted at every position of the five
recognized spellings.

The selector scans bounded byte spans and branches on the chosen
coding without materializing a list of tokens. Optimized Core keeps
the supported set in a machine word. The Criterion allocation
regression measured the same 32 B for every fixture, including the
omitted-field path that simply returns its argument in Core. The Go
1.26.5 fixture applies the same selection rules.

| Preference input | Final Criterion time | Go fixture |
| --- | ---: | ---: |
| Omitted | 3.6 ns | 1.6–1.7 ns |
| Single canonical coding | 11.1 ns | 8.5–9.1 ns |
| Match after an unsupported coding | 17.6 ns | 16.9–17.2 ns |
| Match in a later field with mixed-case coding | 17.5 ns | 16.9–17.9 ns |

GHC 9.10.3 passed 1,089 Hermes, 126 Connect, 95 HTTP/1, 125 HTTP/2,
and 333 aggregate HTTP tests. Connect and aggregate passed under both
schedules. TLS smoke and minimal gRPC passed. Full gRPC passed 91
tests with one existing case pending.

## Connect timeout and protocol version fields

The server requires a single Connect-Timeout-Ms field when present.
Unary POST, unary GET, and streaming calls share the same one-to-ten
ASCII digit grammar. A supplied zero expires before the handler runs.
POST checks the optional Connect-Protocol-Version marker and accepts
one value of `1`. GET checks the optional `connect=v1` query marker.
Empty, repeated, or unsupported version markers produce an invalid
argument error. Streaming errors use EndStreamResponse.
[Connect protocol](https://connectrpc.com/docs/protocol/#unary-rpcs).

Both unary GET clients now emit the timeout field. Unary and streaming
POST use the same timeout and version rendering. Configured durations
are bounded by the ten-digit wire value and the host's microsecond
timer range. Negative durations render as zero. The resolved client
uses the same bound before converting to microseconds.

Six HTTP/1.1 and HTTP/2 loopback cases failed on the earlier code.
They exercise repeated values, invalid versions, suppression of the
optional POST marker, and the wire representation of expired and
very large deadlines. A separate decimal model matched 14,157 parse
cases and 2,116 render cases, including every byte at each position
of representative timeout values.

Core shows one unboxed digit loop followed by the public `Maybe Int`
result. The renderer writes decimal bytes directly. Sequential
Criterion runs and a Go 1.26.5 comparison measured the following
standalone operations. Criterion's allocation includes its runner.
The standalone parser still has a timing gap to the Go fixture.

| Fixture | Previous time / bytes | Current time / bytes | Go time / bytes |
| --- | ---: | ---: | ---: |
| Parse four digits | 5.4 ns / 88 B | 5.6 ns / 72 B | 2.0–2.1 ns / 0 B |
| Parse ten digits | 7.1 ns / 88 B | 8.7 ns / 72 B | 3.7 ns / 0 B |
| Render four digits | 38 ns / 704 B | 11 ns / 264 B | 9.7–10.4 ns / 4 B |
| Render ten digits | 82 ns / 1,568 B | 14 ns / 264 B | 15 ns / 16 B |

GHC 9.10.3 passed 1,089 Hermes, 131 Connect, 95 HTTP/1, 125 HTTP/2,
and 333 aggregate HTTP tests. Connect and aggregate passed under both
schedules. TLS smoke and minimal gRPC passed. Full gRPC passed 91
tests with one existing case pending.

## Connect unary error response headers

The unary client treats only HTTP 200 as RPC success. A non-200 reply
needs a singleton JSON media type and an acceptable content coding
before its body can supply a Connect error. Missing, malformed or
mismatched headers and unreadable error bodies use the HTTP status
mapping. The same rule applies to GET and POST, including protobuf
requests. Resolved calls still expose prefixed trailing metadata.
[Connect unary response](https://connectrpc.com/docs/protocol/#unary-response).

Unary and streaming clients share the response coding check. Identity
is always accepted. Other codings must appear in the caller's
advertisement, or match the request's coding when the advertisement
was omitted. A rejected unary body is consumed for connection reuse
without assembling or decompressing it. The empty accepted body path
skips decompression.

Four HTTP/1.1 and HTTP/2 loopback regressions failed on the earlier
code. The fixtures cover non-200 replies carrying an RPC-shaped
payload, mismatched media types, bad charsets, repeated fields,
unadvertised compression, and valid JSON errors for protobuf calls.
The independent model matched 23,040 status, media type, coding and
configuration combinations.

The optimized worker jumps from the validated media type to coding
selection. The rejected-body worker advances the producer without
accumulating chunks. An exact registered single coding bypasses the
general list parser. Sequential Criterion measurements of the
header decision compare to a Go 1.26.5 fixture using the standard MIME
parser. Criterion allocation includes the runner.

| Response head | Hermes and Connect | Go fixture |
| --- | ---: | ---: |
| HTTP 200 with canonical JSON | 28 ns / 64 B | 27–29 ns / 0 B |
| HTTP 503 with canonical JSON | 28 ns / 64 B | 26–27 ns / 0 B |
| HTTP 503 with canonical JSON and gzip | 41 ns / 152 B | 42–45 ns / 0 B |
| HTTP 503 with parameterized JSON | 328 ns / 2,543 B | 239–240 ns / 376 B |
| HTTP 503 with plain text | 98 ns / 1,000 B | 49–51 ns / 48 B |

GHC 9.10.3 passed 1,089 Hermes, 135 Connect, 95 HTTP/1, 125 HTTP/2,
and 333 aggregate HTTP tests. Connect and aggregate passed under both
schedules. TLS smoke and minimal gRPC passed. Full gRPC passed 91
tests with one existing case pending.

## HTTP cache Vary and 304 metadata

The HTTP client cache uses Hermes' typed `Vary` decoder across every
physical response field line. A wildcard anywhere or a malformed field
prevents storage. Matching compares all request field lines for each
selected name, in wire order, and distinguishes absence from an empty
line. It uses exact field-value bytes, so semantically equivalent wire
spellings can cause a conservative cache miss. Custom stores keep the
same `CacheEntry` field type and preserve the snapshot rows verbatim.
Legacy snapshots with selecting fields miss and can be replaced.
[RFC 9111 §4.1](https://www.rfc-editor.org/rfc/rfc9111.html#section-4.1).

Foreground and background 304 refreshes rebuild freshness metadata and
`Vary` selectors from the merged headers, using the request sent upstream.
They keep the cached body's `Content-Length` and exclude hop-by-hop
updates, including names nominated across repeated `Connection` lines.
A merged `no-store`, wildcard or malformed `Vary` evicts the entry after
validation. Background replacements use the same storage eligibility and
body-size cap as foreground misses. Validator admission and the corrected
response-age calculation still need review.
[RFC 9111 §3.2](https://www.rfc-editor.org/rfc/rfc9111.html#section-3.2).

Six regressions failed with the original cache and all nine new cache
tests passed with the change. A separate list-equality model agreed
across 20,427,540 candidate matches and 3,317 rejected snapshots.
The cases include repeated selectors and lines, empty and absent fields,
opaque bytes, and invalid NUL bytes.

Optimized Core uses recursive list scans, with no allocation attributed to
matching beyond the 32-byte Criterion runner. The Go fixture uses
preselected arrays of string values. The top-level cache lookup, clock,
and response replay are outside these measurements. The old fixture
could ignore later selecting fields or values, and missed absent fields.

| Variant match fixture | Previous time | Current time | Go fixture |
| --- | ---: | ---: | ---: |
| No selectors | 4.8 ns | 6.5 ns | 1.6 ns |
| Selected field absent | 10.1 ns | 16.9 ns | 2.4 ns |
| One selected line | 15.6 ns | 22.0 ns | 4.4 ns |
| Two lines of one field | 15.7 ns | 30.7 ns | 6.5 ns |
| Two selected fields | 16.0 ns | 45.1 ns | 10.8–11.4 ns |

The single-line and two-line snapshot fixtures took 75.9 and 87.4 ns.
The verified no-selector hit took 6.5 ns after replacing two sentinel
comparisons with a single header-name length check. The match still
trails the Go fixture. The checked-in Hermes `vary-parse` and
`vary-repeated` fixtures took 32 and 86 ns in the same session.

GHC 9.10.3 passed 17 focused cache tests, 1,089 Hermes, 342 aggregate
HTTP, 135 Connect, 95 HTTP/1, and 125 HTTP/2 tests. Aggregate HTTP and
Connect passed under both schedules. TLS smoke and minimal gRPC passed.
Full gRPC passed 91 tests with one existing case pending.

## HTTP cache validator selection

At this stage the client cache forwarded all caller preconditions and
range requests to the origin, preserving the field lines. The
[caller condition follow-up](#caller-conditions-on-cache-hits) evaluates
two of those fields on cache hits. The cache bypasses requests with a
nonempty or unknown-length body, so an automatic retry can only replay a
bodyless GET or HEAD. Cache-owned revalidation sends typed singleton
ETag and Last-Modified validators. Invalid or repeated stored validators
are omitted individually. Dates use the HTTP-date renderer and obsolete
input forms are resolved against the time the response was received.
[RFC 9111 §4.3.1 and §4.3.2](https://www.rfc-editor.org/rfc/rfc9111.html#section-4.3.1).

A strong ETag in a 304 requires a strong match. A weak response ETag
compares opaque octets regardless of the cached tag's strength. An entry
validated by date alone can use an identical Last-Modified timestamp.
The cache drains an unidentifiable 304 and sends the original request
once, in the foreground and in stale-while-revalidate. A 304 received
without a cache-owned condition is passed through. Validator matching
ignores hop-by-hop fields. The corrected response-age calculation is
still open.
[RFC 9111 §4.3.4](https://www.rfc-editor.org/rfc/rfc9111.html#section-4.3.4).

Five cache regressions failed on the baseline. Eight new tests passed on
the final build, including a background retry that checks the 304 popper
is drained before the next request. An independent model agreed on 2,700
validator combinations and checked 1,578 request field names. These cases
include malformed and repeated fields, non-UTF-8 ETag octets, optional
field whitespace, and equivalent timestamps with different wire spellings.

Optimized Core uses bounded byte reads for the request field scan,
with no allocation beyond the Criterion runner. The 304 tag check
compares against the canonical spelling rendered from the stored typed
tag. A match validates the whole received field without allocating a
copy of the opaque tag. Go 1.26.5 used a string-based tag validator
and `net/http`'s date parser. The fixtures measure header decisions,
with the transport and cache store outside the timed paths.

| Header decision | Initial implementation | Final Criterion time / allocation | Go fixture |
| --- | ---: | ---: | ---: |
| No caller precondition | 22 ns | 12 ns / 32 B | 8.2–8.5 ns |
| Caller If-None-Match | 26 ns | 13 ns / 32 B | 10–11 ns |
| Caller Range | 27 ns | 15 ns / 32 B | 9.8–10.3 ns |
| Matching strong ETag | 43 ns | 27 ns / 120 B | 14.5–14.8 ns |
| Matching weak ETag | 47 ns | 28 ns / 120 B | 15.0–15.2 ns |
| Different ETag | 32 ns | 16 ns / 120 B | 8 ns |
| Repeated ETag | 22 ns | 12 ns / 32 B | 13.9–14.5 ns |
| Matching Last-Modified | 123 ns | 127 ns / 1,016 B | 78–79 ns |

GHC 9.10.3 passed 25 focused cache tests, 1,089 Hermes, 350 aggregate
HTTP, 135 Connect, 95 HTTP/1, and 125 HTTP/2 tests. Aggregate HTTP and
Connect passed under both schedules. TLS smoke and minimal gRPC passed.
Full gRPC passed 91 tests with one existing case pending.

## HTTP cache age and expiration

The cache records the start and header receipt time of each exchange,
including conditional requests and unconditional retries. Its age is the
greater of the apparent age from Date and the received Age plus request
delay, with resident time added. It uses the first Age member and ignores an
invalid first member. Missing or invalid Date is replaced on storage with
the receipt date. A successful 304 starts from that exchange's Date and
Age, including when either was omitted. Replayed Age is in whole seconds
and saturates at the Hermes codec's `Word32` limit.
[RFC 9110 §6.6.1](https://www.rfc-editor.org/rfc/rfc9110.html#section-6.6.1), [RFC 9111 §4.2.3 and §5.1](https://www.rfc-editor.org/rfc/rfc9111.html#section-4.2.3).

For explicit expiration, `max-age` takes precedence. Otherwise the
lifetime is `Expires` minus `Date`, and duplicate or invalid Expires
does not establish freshness. The current age also governs both stale
extensions. `no-cache` and `must-revalidate` prohibit stale replay.
`immutable` applies only during the freshness lifetime. Stale-if-error
uses the specified 500, 502, 503 and 504 statuses and transport errors.
[RFC 9111 §4.2.1 and §4.2.4](https://www.rfc-editor.org/rfc/rfc9111.html#section-4.2.1), [RFC 8246 §2](https://www.rfc-editor.org/rfc/rfc8246.html#section-2), [RFC 5861 §4](https://www.rfc-editor.org/rfc/rfc5861.html#section-4).

The built-in cache decodes initial age and lifetime before inserting the
entry. Hits add resident time to that snapshot. The cache derives timing
from a custom backend's `CacheEntry` after its variant key matches. GHC Core shows the
same-day path subtracting picoseconds directly and retaining the general
UTC difference for day boundaries. A reference matched 16,128 age,
clock and lifetime combinations. The UTC fast path matched the time
library on 1,600 pairs, including leap seconds and day boundaries.

Sequential Criterion fixtures measure freshness arithmetic and the
decoding that it needs. The Go 1.26.5 fixture uses `http.ParseTime` and
nanosecond duration arithmetic. Measurements exclude the cache store,
the system-clock call, and response body replay. Criterion allocations
include the runner.

| Timing fixture | Initial implementation | Final Criterion time / allocation | Go fixture |
| --- | ---: | ---: | ---: |
| Stored timing hit | 418 ns with field decoding | 14 ns / 64 B | 4.6 ns |
| Read raw entry without Date | 274 ns | 45 ns / 96 B | 15–16 ns |
| Read raw entry with Date | 405 ns | 164 ns / 1,096 B | 90–91 ns |
| Read raw entry with Date and Age | 418 ns | 171 ns / 1,192 B | 101–103 ns |
| Read raw entry expiring via Expires | 470 ns | 171 ns / 1,208 B | 101 ns |

The Age rendering fixture fell from 45 ns and 544 bytes to 16 ns and
336 bytes. Same-day timestamp experiments with a per-entry age offset and
with forced inlining each measured 13–14 ns, so the entry-time arithmetic
still trails the Go fixture.

Seven regressions failed on the baseline. All nine new tests passed on
the final build. GHC 9.10.3 passed 34 focused cache tests, 1,089 Hermes,
359 aggregate HTTP, 135 Connect, 95 HTTP/1, and 125 HTTP/2 tests. Aggregate
HTTP and Connect passed under both schedules. TLS smoke and minimal gRPC
passed. Full gRPC passed 91 tests with one existing case pending.

## HTTP cache request directives

The client cache parses request controls once and uses corrected age to
enforce `max-age`, `min-fresh` and `max-stale`. Across repeated directives,
the smallest age and staleness limits and the largest minimum freshness
apply. The cache honours response `no-cache` and `must-revalidate` when
considering stale reuse. A request `stale-if-error` window can permit an
error fallback. If the response supplies a window too, the shorter one
applies. `Pragma: no-cache` is a compatibility fallback for requests
without any `Cache-Control` field.
[RFC 9111 §5.2.1 and §5.4](https://www.rfc-editor.org/rfc/rfc9111.html#section-5.2.1), [RFC 5861 §4](https://www.rfc-editor.org/rfc/rfc5861.html#section-4).

An `only-if-cached` request gets an eligible matching variant or a local
empty 504. This also covers origin-only requests, conflicting controls,
and variant misses. A response eligible under `stale-while-revalidate`
can be served to a cache-only request with the background refresh
suppressed. Request `no-store` bypasses lookup and insertion. A request
freshness constraint can trigger an ordinary validator round trip.

Hermes' single-field fast paths for bare `no-cache`, `no-store`,
`only-if-cached` and short `max-age` values share the same typed results
across raw parsing and `KnownHeader` lookup. The general grammar handles
compound values and alternative spellings. The fast paths matched the
general grammar on 4,429 inputs. An independent request policy model
agreed across 138,240 freshness and stale-window combinations.

The no-control scan checks folded header-name length and first byte
before string comparison. Core shows a recursive scan using bounded byte
reads. The previous fixture only checked `no-store` and `no-cache` and
parsed cache controls separately for each. The current fixture parses
once and chooses reuse or revalidation against a precomputed age and
lifetime. Go 1.26.5 uses a token parser for the canonical fixtures.

| Request fixture | Previous Criterion time / allocation | Current Criterion time / allocation | Go fixture |
| --- | ---: | ---: | ---: |
| No cache controls | 34 ns / 112 B | 16 ns / 48 B | 3.2 ns |
| `no-cache` | 119 ns / 1,120 B | 25 ns / 112 B | 15 ns |
| `max-age=30` | 101 ns / 944 B | 36 ns / 256 B | 18 ns |
| Repeated Cache-Control fields | 254 ns / 2,256 B | 154 ns / 1,392 B | 71 ns |
| `Pragma: no-cache` | 40 ns / 112 B | 22 ns / 72 B | 11 ns |
| `only-if-cached` | 95 ns / 880 B | 26 ns / 112 B | 19 ns |

The checked-in compound Cache-Control parsing fixtures used the same
allocation as the baseline. The canonical list took 62 ns before and
65 ns after, the qualified list took 135 and 127 ns, and the mixed-case
list took 57 ns in both runs.

Six middleware regressions failed on the baseline. GHC 9.10.3 passed all
seven new cache tests and the new Hermes codec test, bringing the totals to
41 focused cache, 1,090 Hermes, 366 aggregate HTTP, 135 Connect, 95 HTTP/1,
and 125 HTTP/2 tests. Aggregate HTTP and Connect passed under both
schedules. TLS smoke and minimal gRPC passed. Full gRPC passed 91 tests
with one existing case pending.

## HTTP cache invalidation after writes

A 2xx or 3xx response to an unsafe or unknown method invalidates both the
GET and HEAD entries for its target URI. Cache hits use an ASCII-folded
host key and the effective port. The middleware uses the registered safety
of `QUERY`, `PROPFIND`, `REPORT`, `SEARCH`, and `PRI` in addition to the
RFC 9110 methods. Transport failures and 4xx or 5xx responses leave stored
responses eligible for ordinary cache policy.
[RFC 9111 §4.4](https://www.rfc-editor.org/rfc/rfc9111.html#section-4.4), [IANA HTTP Method Registry](https://www.iana.org/assignments/http-methods/http-methods.xhtml).

The cache also invalidates same-origin targets found in a valid singleton
`Location` or `Content-Location`. It resolves relative, network-path and
HTTP(S) references, retains the resolved query, and drops fragments from
the key. Query-only references preserve the base path, and an explicit
empty query clears the base query. The shared URI parser uses Hermes'
bounded port decoder and extracts the interior of bracketed IPv6 hosts.
[RFC 3986 §5](https://www.rfc-editor.org/rfc/rfc3986.html#section-5).

The built-in store advances its generation in the same transaction as
invalidation. A request or revalidation begun before a generation change
can return its response to its caller. It cannot publish it into the
cache, even for an unrelated key. `newSynchronizedCacheWith` orders
publication and invalidation for a custom store within one cache
instance. A backend shared across instances or processes needs coordination
at the storage layer. The pure `newCacheWith` uses the store callbacks
directly.

Five regressions reproduced on the baseline. The final tests include an
in-flight miss and an in-flight 304 held at body consumption while a
write completes, plus the coordinated custom-store case. An independent
check covered 41 RFC-style URI resolution examples, ten invalidation
plans, and 65,637 port values, including every representable HTTP port.

The paired warm-GET Criterion fixture uses a mock transport and the same
client URI parser for both builds. It took 353 ns before and 358 ns after,
with about 3.55 KB allocated in both runs. Key selection is measured
separately from the store. The Go 1.26.5 fixture uses `net/url` resolution
and canonical field names. The Criterion allocations include its runner.

| Invalidation fixture | Initial Criterion time / allocation | Tuned Criterion time / allocation | Go fixture |
| --- | ---: | ---: | ---: |
| Primary target only | 63 ns / 808 B | 56 ns / 648 B | 25–27 ns |
| Two relative fields | 502 ns / 4,087 B | 485 ns / 3,727 B | 559–581 ns |
| Two absolute fields | 716 ns / 5,543 B | 658 ns / 5,279 B | 504–505 ns |

Optimized Core constructs the two method keys directly for each selected
target. The `Location` relative parser fixture took 50 ns and 376 B.
Method safety took 4 ns for GET and 6 ns for PROPFIND on the local fixtures.

GHC 9.10.3 passed 46 focused cache tests, 1,090 Hermes, 372 aggregate
HTTP, 135 Connect, 95 HTTP/1, and 125 HTTP/2 tests. Aggregate HTTP and
Connect passed under both schedules. TLS smoke and minimal gRPC passed.
Full gRPC passed 91 tests with one existing case pending.

## HTTP redirect Location and URI resolution

The redirect middleware follows a valid singleton typed `Location`.
Absent, repeated, malformed, non-HTTP(S), and connection-scoped values
leave the original response and its body available to the caller.
Commas and percent escapes in valid URI references retain their spelling.
[RFC 9110 §10.2.2](https://www.rfc-editor.org/rfc/rfc9110.html#section-10.2.2).

Redirects and cache invalidation share the HTTP URI-reference resolver.
Query-only references preserve the base path. Redirects inherit the
previous fragment only when the reference omits its fragment delimiter.
An absolute HTTP(S) reference works with an unknown base. Relative and
network-path references need a known base. A bare `?` clears an inherited
query. At this checkpoint the client URI model elided the bare delimiter.
See the [delimiter follow-up](#empty-query-and-fragment-delimiters).
[RFC 3986 §5](https://www.rfc-editor.org/rfc/rfc3986.html#section-5),
[RFC 3986 §6.2.3](https://www.rfc-editor.org/rfc/rfc3986.html#section-6.2.3).

The 301 and 302 rewrite option applies to POST. A 303 follows with HEAD
for HEAD requests and GET for other methods. Discarding a payload also
removes its representation and framing fields. On an origin change the
client clears an explicit Host and applies the existing Authorization
and Cookie policies. Loop detection keys on the method and normalized
request URI without userinfo or fragments. The hop limit counts
transitions and allows a terminal response at the limit.
[RFC 9110 §15.4](https://www.rfc-editor.org/rfc/rfc9110.html#section-15.4).

The original seven regressions failed on the baseline. The final run
passed 17 redirect tests and 46 cache tests. The shared resolver and
invalidation path passed 41 RFC-style reference examples, ten independent
invalidation plans, and 65,637 port cases. GHC 9.10.3 passed 1,090 Hermes,
381 aggregate HTTP, 135 Connect, 95 HTTP/1, and 125 HTTP/2 tests.
Aggregate HTTP and Connect passed under both schedules. TLS smoke and
minimal gRPC passed. The full gRPC suite passed 91 tests with one pending.

Sequential Criterion runs compared the original and revised middleware
with the same compiled URI library and a scripted transport. The final
path builds a single literal URI-template segment after validation.
Simplified Core leaves the initial URI parse suspended until a redirect
needs it and contains no second template parse after resolution.

| Mock transport fixture | Original | Initial typed path | Tuned typed path |
| --- | ---: | ---: | ---: |
| Terminal GET | 41 ns / 784 B | 39 ns / 760 B | 40 ns / 760 B |
| One relative redirect | 1,207 ns / 10,024 B | 1,599 ns / 11,671 B | 851 ns / 8,423 B |
| One absolute redirect | 1,245 ns / 10,344 B | 1,538 ns / 12,143 B | 925 ns / 8,895 B |

Selecting and materializing a target from response headers took 298 ns
and 2,728 B for the relative fixture and 345 ns and 3,200 B for the
absolute fixture. The Go 1.26.5 implementation uses `net/url` and a
pre-parsed base. It took 226–268 ns and 368 B for relative, and 221–225 ns
and 360 B for absolute. Hermes additionally checks the RFC 3986 field
grammar and constructs its request URI template. The allocation gap
remains for both fixtures. The checked-in `location-relative` codec
fixture took 54 ns and 376 B, and `location-render` took 13 ns and 264 B.

## Empty query and fragment delimiters

The URI parser retains both presence and bytes for query and fragment
components. `Location: ?` resolves to a target with an explicitly empty
query. HTTP/1 emits the final question mark in the request line, HTTP/2
emits it in `:path`, and the WAI client adapter reports it in
`rawQueryString`. Fragments remain in the URI and are omitted from
requests. An explicit `#` replaces a prior fragment with an empty one
and can carry through a later redirect that omits its fragment.
Cache lookup and invalidation keep `/record` and `/record?` distinct.
[RFC 3986 §6.2.3](https://www.rfc-editor.org/rfc/rfc3986.html#section-6.2.3).

The six-field `URI` constructor, record interface, and
`URI (..)` imports still compile through a bundled pattern. Its
byte-oriented query and fragment fields project absence as empty
bytes, and writes through that pattern construct empty components
as absent. The `uriQueryComponent` and `uriFragmentComponent` fields
retain the distinction and are used by the resolver and redirect
middleware.

Four baseline regressions reproduced delimiter loss in redirects,
cache lookup, and the two wire protocols. The final local run passed
17 redirect, 47 cache, 18 WAI, 1,090 Hermes, 384 aggregate HTTP, and
135 Connect tests. Aggregate HTTP and Connect passed under both
schedules. HTTP/1, HTTP/2, TLS smoke, and minimal gRPC passed. The full
gRPC suite passed 91 tests with one pending. An independent check
covered 12 query and fragment combinations across absolute, network-path
and relative references, 41 RFC-style examples, ten invalidation plans,
and 65,637 port values. It also checked legacy record construction and
IDNA conversion with empty delimiters.

Criterion compiled both versions with GHC 9.10.3 and forced the emitted
request target. Go 1.26.5 used `net/url.Parse` and `URL.RequestURI` on the
same inputs. The changed path compares canonical schemes as bytes.
Optimized Core keeps the case-folding fallback off canonical inputs and
eliminates the intermediate target-parts constructor at the parse sites.

| Parse and emit target | Previous | Current | Go |
| --- | ---: | ---: | ---: |
| Absent query | 90 ns / 1,112 B | 43 ns / 328 B | 92–104 ns / 144 B |
| Nonempty query | 111 ns / 1,272 B | 63 ns / 472 B | 112–120 ns / 160 B |
| Bare query delimiter | 96 ns / 1,176 B | 53 ns / 424 B | 113–115 ns / 152 B |

With a pre-parsed base, resolving and rendering the representative
relative reference went from 155 ns and 1,104 B to 144 ns and 1,136 B.
The corresponding absolute reference went from 180 ns and 1,656 B to
127 ns and 904 B. Resolving a bare question mark went from 49 ns and
312 B to 51 ns and 408 B and now emits the delimiter.

## Retry-After in the retry middleware

The retry middleware uses the typed singleton field on responses
selected by the retry policy. Malformed, repeated and connection-scoped
values use the local backoff. Delta-seconds are bounded by the policy
before conversion to microseconds, including the full representable
`Word` range. The absolute date form uses the wall clock for the
RFC 850 century window and rounds a positive sub-microsecond remainder
up. An absent field, a disabled policy and a delta-seconds value
require no clock read.
[RFC 9110 §10.2.3](https://www.rfc-editor.org/rfc/rfc9110.html#section-10.2.3),
[RFC 6585 §4](https://www.rfc-editor.org/rfc/rfc6585.html#section-4).

The client drains the intermediate response before waiting and leaves
the final response available to its caller. Both the initial and the
scaled client backoff use the same cap. At this stage the retry policy
also clamped the server delay. The [retry budget follow-up](#retry-after-and-retry-budgets)
returns the response when a valid delay exceeds that cap.
`withRetryUsing` supplies a clock and wait operation for deterministic
tests and callers with their own scheduler.

Seven focused cases failed on the baseline. The final GHC 9.10.3 run
passed 15 focused retry, 391 aggregate HTTP, 1,090 Hermes, 135 Connect,
18 WAI, 95 HTTP/1 and 125 HTTP/2 tests. Aggregate HTTP and Connect passed
under both schedules. TLS smoke and minimal gRPC passed. The full gRPC
suite passed 91 tests with one pending. An independent integer model
checked 70,035 delta and cap combinations and 10,001 absolute-date
rounding cases.

The Criterion fixture measures header lookup and conversion with a
fixed clock, excluding the actual wait. Simplified Core has no
clock call in the numeric and absent branches. The exact date
conversion uses one integer quotient to round picoseconds to
microseconds. The Go 1.26.5 fixture uses a prebuilt HTTP header map
and `http.ParseTime`. Hermes also checks weekday consistency, rejects
connection-scoped fields, and supports the rolling century window.

| Delay fixture | Previous | Current | Go |
| --- | ---: | ---: | ---: |
| No field | 7.9 ns / 0 B | 7.5 ns / 0 B | 14–16 ns / 0 B |
| One second | 31 ns / 480 B | 35 ns / 336 B | 19 ns / 0 B |
| Capped 3,600 seconds | 34 ns / 480 B | 36 ns / 320 B | 21 ns / 0 B |
| HTTP-date | 246 ns / 1,816 B | 200 ns / 1,456 B | 118–122 ns / 80 B |

The checked-in typed delta-seconds fixture took 20 ns and 304 B.
See the [redirect follow-up](#retry-after-on-redirects) for the minimum
wait on automatic redirects.

## Retry-After on redirects

Automatic 3xx redirects use the same typed singleton field reader as
the retry middleware. Valid delay-seconds and HTTP-dates set a
minimum wait. The default cumulative wait budget is five seconds
per redirect chain, configurable with `withRedirectsWithin`.
The budget counts scheduled waits. If a minimum exceeds the remaining
budget, the middleware returns that redirect with its original body
available. Accepted intermediate responses are drained before waiting.
Absent, malformed or connection-scoped fields supply no delay, and
past dates and zero-second values proceed immediately. Redirects with
unusable targets do not reach the delay reader.
[RFC 9110 §10.2.3](https://www.rfc-editor.org/rfc/rfc9110.html#section-10.2.3).

The shared reader distinguishes an over-budget delay from an absent
or malformed field. `withRedirectsUsing` supplies a wall clock and a
microsecond wait function for deterministic scheduling tests. See the
[large delay follow-up](#large-retry-after-delays) for values beyond
`Word`.

The baseline ignored delays and followed redirects beyond the
configured wait budget. The final GHC 9.10.3 run passed 20 redirect
and 15 retry tests, 394 aggregate HTTP, 1,090 Hermes, 135 Connect,
18 WAI, 95 HTTP/1, and 125 HTTP/2 tests. Aggregate HTTP and Connect
passed under both schedules. TLS smoke and minimal gRPC passed.
Full gRPC passed 91 tests with one pending. An independent model
checked 80,040 delta-seconds and cap combinations and 20,002
absolute-date cases at the budget boundary.

Sequential Criterion runs used a scripted transport, a fixed clock
and a no-op wait. They measure the scheduling path without the actual
sleep. Simplified Core keeps the delay reader behind status and
target validation. It reads the clock on the date path.

| Transport fixture | Before | After |
| --- | ---: | ---: |
| Terminal GET | 40 ns / 760 B | 42 ns / 784 B |
| One relative redirect without a delay | 820 ns / 7,568 B | 836 ns / 7,624 B |
| One relative redirect with delta-seconds | 814 ns / 7,568 B | 889 ns / 7,960 B |
| One relative redirect with an HTTP-date | 804 ns / 7,568 B | 1,082 ns / 9,023 B |

The shared delay reader on its own took 39 ns and 336 B for one
second, and 196 ns and 1,400 B for an HTTP-date. The same Go
1.26.5 header map fixture used in the retry pass took 19–20 ns and
0 B, and 117–120 ns and 80 B, respectively. The Hermes path
validates the singleton field and checks its connection scope
and calendar day.

## Large Retry-After delays

The `delay-seconds` grammar accepts nonempty decimal digits.
The existing `DelaySeconds Word` constructor handles numbers that fit.
`LargeDelaySeconds` retains overflow values as validated,
compact decimal digits. Both render in canonical decimal form without
leading zeros. The `parseRetryAfterDelay` constructor accepts numeric
wire values with field whitespace, and `largeDelayDigits` exposes the
canonical digits without converting to an arbitrary-precision integer.
`SpecificTime` retains the existing HTTP-date form.
[RFC 9110 §10.2.3](https://www.rfc-editor.org/rfc/rfc9110.html#section-10.2.3).

An out-of-range machine-word delay always exceeds a microsecond
budget representable as `Int`. The shared reader classifies it
as above the limit without integer conversion or a clock read.
Redirects return the original response when the wait exceeds their
remaining budget. The [retry budget follow-up](#retry-after-and-retry-budgets)
applies the same rule to retries.
Signed values, embedded whitespace and trailing non-digits remain
invalid, including after a long run of digits.

Three baseline checks reproduced failures at the typed field,
retry and redirect entry points. GHC 9.10.3 passed 1,091 Hermes,
394 aggregate HTTP, 15 retry, 20 redirect, 135 Connect, 18 WAI,
95 HTTP/1, and 125 HTTP/2 tests. Aggregate HTTP and Connect passed
under both schedules. TLS smoke and minimal gRPC passed. The full
gRPC suite passed 91 tests with one pending. An independent decimal
model checked 123,530 parse and render cases, including every byte
value in each position of a packed scan block and values around the
Word boundary.

Short bare decimals use the shared bounded fast path. Long decimals
use the packed ASCII scanner and copy only their canonical digits.
Optimized Core checks for eight available bytes before each packed
load and allocates the compact result after scanning.

| Parse fixture | Previous codec | Initial large-delay codec | Tuned codec | Go 1.26.5 |
| --- | ---: | ---: | ---: | ---: |
| Four-digit delay | 19 ns / 304 B | 19 ns / 304 B | 8.6 ns / 120 B | 9.0–9.5 ns / 0 B |
| 21-digit delay | Rejected | 45 ns / 400 B | 39 ns / 440 B | 65–69 ns / 96 B |
| 4,096-digit delay | Rejected | 1,922 ns / 4,472 B | 471 ns / 4,512 B | 1,367–1,415 ns / 8,240 B |
| HTTP-date | 144 ns / 1,160 B | 141 ns / 1,160 B | 142 ns / 1,200 B | |

The Go fixture calls `strconv.ParseUint` and, on range overflow,
validates and copies the decimal string. The checked-in Criterion
fixtures measured 9 ns for four digits and 473 ns for 4,096 digits.

## Last-Event-ID on SSE reconnects

The reconnecting client seeds its cursor from a valid singleton
`Last-Event-ID` and renders each replay through Hermes. Malformed
and repeated seeds are dropped. An explicit `Connection` option
naming the field suppresses replay. Each blank line commits the
current cursor, including when a block has an ID and no data.
An empty ID omits the next request header, and an incomplete block
at disconnect leaves the previous cursor in place.
[HTML §9.2.3–6](https://html.spec.whatwg.org/multipage/server-sent-events.html#the-last-event-id-header).

A reconnect seeds the stream parser with the committed ID. This
allows a first event without an ID to inherit the previous cursor
and matches Chromium's parser initialization. Event IDs are UTF-8.
An ID field containing NUL is ignored. Malformed UTF-8 subsequences
are replaced with U+FFFD before the ID is dispatched or used
as a header.
[Chromium EventSource parser](https://raw.githubusercontent.com/chromium/chromium/main/third_party/blink/renderer/modules/eventsource/event_source_parser.cc),
[WHATWG UTF-8 decoder](https://encoding.spec.whatwg.org/#utf-8-decoder).

Four reconnect regressions failed on the baseline. GHC 9.10.3
passed 83 SSE, 398 aggregate HTTP, 1,091 Hermes, 135 Connect,
18 WAI, 95 HTTP/1 and 125 HTTP/2 tests. Aggregate HTTP and Connect
passed under both schedules. TLS smoke and minimal gRPC passed.
Full gRPC passed 91 tests with one pending.

Node's WHATWG `TextDecoder` agreed on all 134,546 UTF-8 inputs,
including every one- and two-byte sequence, truncated code points,
the byte order mark inside an ID, and 65,536 consecutive invalid
bytes. The SSE parser preserved the expected events at all 51
single split points in a CRLF stream containing malformed and empty IDs.

In optimized Core, valid bytes take the bytestring validator
branch and retain the original slice. The repair builder is called
only for malformed input. Valid ID validation took 6.8 ns and 48 B
for the ASCII fixture and 7.7 ns and 48 B for the UTF-8 fixture.
Go 1.26.5 `utf8.Valid` took 5.3–5.7 ns and 4.7–5.0 ns respectively,
with no allocation in either fixture.

| Parse 32 events | Before | After |
| --- | ---: | ---: |
| Data only | 2.38 µs / 45,888 B | 2.32 µs / 46,416 B |
| ASCII IDs | 3.46 µs / 63,808 B | 3.76 µs / 66,896 B |
| UTF-8 IDs | 3.44 µs / 63,808 B | 3.75 µs / 66,896 B |

The checked-in `SSE cursor` Criterion group covers the same three fixtures.

## Multipart byte-range responses

The client requires an opening delimiter and a complete closing delimiter.
The opening line selects CRLF or legacy LF framing. Each delimiter consumes
its own preceding line break, preserving any earlier payload bytes. Boundary
text inside a binary part, transport padding, preambles, and epilogues are
handled independently. MIME unfolding removes the line break and preserves
the continuation whitespace, including inside quoted media parameters.
[RFC 9110 §8.3.3](https://www.rfc-editor.org/rfc/rfc9110.html#section-8.3.3),
[RFC 9110 §14.6](https://www.rfc-editor.org/rfc/rfc9110.html#section-14.6),
[RFC 2046 §5.1.1](https://www.rfc-editor.org/rfc/rfc2046.html#section-5.1.1),
[RFC 5322 §2.2.3](https://www.rfc-editor.org/rfc/rfc5322.html#section-2.2.3).

A returned part has a single satisfied byte `Content-Range` and exactly the
number of bytes it declares. The length check avoids overflow at the full
`Word64` span. An optional `Content-Type` is checked with Hermes's typed
media grammar. The parser returns raw slices, accepts identity transfer
encodings, and skips unusable parts in a correctly framed response.

Five framing and metadata regressions failed on the initial baseline.
Three additional cases exposed binary line-ending and MIME unfolding
issues during development. GHC 9.10.3 passed 30 range, 406 aggregate HTTP,
1,091 Hermes, 135 Connect, 18 WAI, 95 HTTP/1 and 125 HTTP/2 tests. Aggregate
HTTP and Connect passed under both schedules. TLS smoke and minimal gRPC
passed. Full gRPC passed 91 tests with one existing pending case. Go's
`mime/multipart` generated and read 1,024 responses containing 3,070 parts.
Hermes matched every part and rejected all 1,024 truncated envelopes.

Optimized Core keeps the scan offsets unboxed and calls the shared SIMD
scanner for LF and dash. Both searches skip delimiter-free spans. Payloads
are sliced from the input and allocated bytes stay flat as part size grows.
The Go fixture streams the same inputs through `mime/multipart` and validates
each part's media type and byte count.

| Parse fixture | Previous client | Tuned client | Go 1.26.5 |
| --- | ---: | ---: | ---: |
| Two short parts | 2.09 µs / 5,288 B | 0.74 µs / 6,224 B | 1.33–1.43 µs / 5,928 B |
| Two 64 KiB parts | 654 µs / 5,292 B | 3.14 µs / 6,224 B | 35.1–38.8 µs / 5,952 B |
| Two 64 KiB line-oriented parts | 676 µs / 5,292 B | 3.15 µs / 6,224 B | 19.6–19.9 µs / 5,952 B |
| Two 64 KiB LF-filled parts | 658 µs / 5,292 B | 3.14 µs / 6,224 B | 34.8–35.2 µs / 5,952 B |

The checked-in `Multipart ranges` Criterion group covers short, 64 KiB,
and 64 KiB LF-filled payloads.

## Connect timeout decoding performance

`Connect-Timeout-Ms` decodes four or eight ASCII digits per packed load
when enough bytes remain, then checks any remaining digits individually.
Each load supports unaligned field slices. The buffer stays live through
the packed and scalar reads. The host timer bound still applies on narrow
hosts. [Connect protocol](https://connectrpc.com/docs/protocol/#unary-rpcs).

An independent integer model matched 1,467,452 cases, including all
adjacent byte pairs at every offset in four-, eight-, and ten-digit
fixtures, every wire length through the limit, and unaligned slices.
GHC 9.10.3 passed 137 Connect, 1,091 Hermes, 406 aggregate HTTP, 18 WAI,
95 HTTP/1, and 125 HTTP/2 tests. Connect and aggregate HTTP passed under
both schedules. TLS smoke and minimal gRPC passed. Full gRPC passed
91 tests with one existing pending case.

The local Criterion comparison forces the public `Maybe Int` result.
The second comparison immediately consumes each value in a batch of 512
distinct inputs. Optimized Core uses unboxed offsets and accumulators,
and the batch allocates about 48 bytes total with either implementation.
The Go 1.26.5 comparator implements the same decimal grammar and returns
the numeric value and a validity flag.

| Parse fixture | Previous parser | Packed parser | Go 1.26.5 |
| --- | ---: | ---: | ---: |
| Four digits, standalone | 8.50 ns / 88 B | 6.20 ns / 64 B | 2.02–2.11 ns |
| Ten digits, standalone | 11.21 ns / 88 B | 6.98 ns / 64 B | 3.88–4.03 ns |
| Four digits, consumed batch | 2.77 ns/field | 2.21 ns/field | 2.09–2.13 ns/field |
| Ten digits, consumed batch | 5.25 ns/field | 3.18 ns/field | 4.25–4.34 ns/field |

## Authoring conditional range requests

The client `withRange` combinator replaces earlier Range lines. An empty
list or reversed interval clears both Range and If-Range. `ifRange` uses
the Hermes wire renderer. The base, pooled, and streamed transports
validate the pair after request modifiers and middleware, so callers can
apply the two combinators in either order.

An If-Range field is sent with a single syntactically valid Range field and
one strong entity tag or HTTP-date. A weak or malformed tag, ambiguous
field lines, or an absent or malformed range clears both fields, requesting
the full representation. A caller using a date is responsible for knowing
that the representation has no entity tag and that the date is a strong
validator. [RFC 9110 §13.1.5](https://www.rfc-editor.org/rfc/rfc9110.html#section-13.1.5),
[RFC 9110 §14.1.1](https://www.rfc-editor.org/rfc/rfc9110.html#section-14.1.1).

HTTP/1.1 loopback checks verified both modifier orders, a range without a
validator, and fallback from weak tags, malformed tags, orphaned validators,
reversed ranges, and repeated fields. The focused suites passed 31 range
and 18 conditional tests. GHC 9.10.3 passed 410 aggregate HTTP, 1,091 Hermes,
137 Connect, 18 WAI, 95 HTTP/1, and 125 HTTP/2 tests. Aggregate HTTP and
Connect passed under both schedules. TLS smoke and minimal gRPC passed.
Full gRPC passed 91 tests with one existing pending case.

On the three-header ordinary request fixture, transport preparation went
from 28 to 37 ns with 192 bytes allocated in both runs. Preparation with a
valid conditional range went from 36 to 143 ns and from 192 to 1,136 bytes.
Core shows that the ordinary path reuses the existing header list. Typed
validation and its allocations occur only when If-Range is present. The
checked-in authoring benchmark measured about 30 ns each for replacing
the range or replacing its validator.

## Conditional range redirects

An automatic redirect clears both Range and If-Range when the target
origin, wire path, query, or method changes. Entity tags and strong dates
are scoped to a resource. A new resource can use the same validator for
different bytes. An unconditional Range continues across redirects. For
the same origin, target, and method, fragment changes preserve the pair.
The comparison distinguishes an absent query from an explicitly empty
one and compares path spellings as sent on the wire.
[RFC 9110 §8.8.1](https://www.rfc-editor.org/rfc/rfc9110.html#section-8.8.1),
[RFC 9110 §13.1.5](https://www.rfc-editor.org/rfc/rfc9110.html#section-13.1.5).

Two scripted regressions failed on the baseline. An HTTP/1.1 loopback
regression also reproduced a 206 from a different resource whose entity
tag matched the source resource's tag. The revised client received the
complete 200 response. GHC 9.10.3 passed 24 redirect, 415 aggregate HTTP,
1,091 Hermes, 137 Connect, 18 WAI, 95 HTTP/1, and 125 HTTP/2 tests.
Aggregate HTTP and Connect passed under both schedules. TLS smoke and
minimal gRPC passed. Full gRPC passed 91 tests with one existing pending
case.

The path and query check runs only when the request contains If-Range.
Core compares the existing URI components, and the ordinary same-origin
path reuses the original field list. Sequential Criterion runs against an
in-memory peer measured the ranges below across two paired runs. The
checked-in `Redirected ranges` group covers the same-origin fixtures.

| Follow one redirect | Previous middleware | Current middleware |
| --- | ---: | ---: |
| Ordinary same-origin GET | 599–636 ns / 6,192 B | 631–656 ns / 6,264 B |
| Ordinary cross-origin GET | 734–745 ns / 6,648 B | 754–784 ns / 6,720 B |
| Conditional range on the same origin | 613–630 ns / 6,192 B | 638–669 ns / 6,320 B |

## Content coding and ranged responses

The client advertises `identity` for a Range request when the caller
omits Accept-Encoding. Explicit preferences, including an empty value,
remain in effect. The middleware returns a 206 with its original bytes,
Content-Encoding, Content-Length, and Content-Range. Byte offsets are
defined over the encoded representation. A complete coded 200 response
to the same request still goes through the decoder.
[RFC 9110 §12.5.3](https://www.rfc-editor.org/rfc/rfc9110.html#section-12.5.3),
[RFC 9110 §14.1.2](https://www.rfc-editor.org/rfc/rfc9110.html#section-14.1.2).

HEAD, informational, 204, 205, 304, and successful CONNECT responses
pass through without installing a content decoder. This preserves
representation metadata on HEAD and 304. An identity-only coding list
requires no decoder and leaves the fields intact. An empty handler list
passes the transport through. [RFC 9110 §8.6](https://www.rfc-editor.org/rfc/rfc9110.html#section-8.6).

Four focused cases and the HTTP/1.1 gzip range regression failed on the
baseline. The wire test now verifies the requested coding and the exact
gzip slice, plus an identity-coded range when Accept-Encoding is omitted
by the caller. GHC 9.10.3 passed 14 decompression, 421 aggregate HTTP,
1,091 Hermes, 137 Connect, 18 WAI, 95 HTTP/1, and 125 HTTP/2 tests.
Aggregate HTTP and Connect passed under both schedules. TLS smoke and
minimal gRPC passed. Full gRPC passed 91 tests with one existing pending
case.

The request scans the existing field list once to choose an advertisement
or preserve the caller's field. Optimized Core keeps that scan in a join
loop. Sequential Criterion runs with an in-memory transport measured the
following. The 206 and HEAD comparisons force response metadata without
pulling bytes. The checked-in `Decompression policy` group also drains a
preserved gzip slice.

| Fixture | Previous middleware | Current middleware |
| --- | ---: | ---: |
| Unencoded GET, drained | 39–40 ns / 672 B | 39–40 ns / 672 B |
| 64 KiB gzip GET, drained | 15.1–15.2 µs / 204,056 B | 15.0–15.7 µs / 204,039 B |
| Gzip 206, metadata | 64–67 ns / 904 B | 23 ns / 288 B |
| Gzip HEAD, metadata | 66–69 ns / 904 B | 22–23 ns / 288 B |

## Body metadata after cache revalidation

A successful 304 refreshes cache metadata without replacing the stored
body. The merge retains the stored Content-Length, Content-Encoding,
and Content-Range fields, including their absence. This lets a cache
above decompression replay decoded bytes without regaining the wire's
coding or length, and lets a raw cache retain the coding chain that
describes its stored bytes. Eligible updates preserve their field-line
order. [RFC 9111 §3.2](https://www.rfc-editor.org/rfc/rfc9111.html#section-3.2),
[RFC 9110 §14.4](https://www.rfc-editor.org/rfc/rfc9110.html#section-14.4).

Both new cache regressions failed on the baseline. The composed cache
and decompressor now replay decoded bytes with the correct metadata.
A raw cache keeps its original gzip and Brotli coding lines and applies
fresh Cache-Control and Content-Type. Both refreshed entries serve the
next request from cache. The merge matched a reference on 65,536 field
subsets, including repeated mixed-case names and Age and Date updates.

GHC 9.10.3 passed 49 cache, 423 aggregate HTTP, 1,091 Hermes, 137 Connect,
18 WAI, 95 HTTP/1, and 125 HTTP/2 tests. Aggregate HTTP and Connect
passed under both schedules. TLS smoke and minimal gRPC passed. Full
gRPC passed 91 tests with one existing pending case.

The merge checks field membership directly. Optimized Core discards the
intermediate name list and calls the specialized header lookup. Criterion
measured an ordinary metadata merge at 302 ns and 1,872 B before the
change and 151 ns and 552 B after. The decoded-body fixture with coding,
length, and range in the update went from 382 to 171 ns and from 2,352 to
552 B. The checked-in `Cache body metadata` group seeded and revalidated
a decoded entry in 3.41 µs through the middleware.

## Digests across content coding and revalidation

The compression middleware clears Content-Digest, Repr-Digest, Content-MD5,
and Digest when it encodes a request or decodes a response. These fields
can describe the original coded bytes. Digest preferences and the origin
ETag still reach their consumers. Identity and already encoded requests
keep their original fields, as do responses the decoder passes through,
including partial responses. [RFC 9530 §2](https://www.rfc-editor.org/rfc/rfc9530.html#section-2),
[RFC 9530 Appendix B.4](https://www.rfc-editor.org/rfc/rfc9530.html#appendix-B.4),
[RFC 2616 §14.15](https://www.rfc-editor.org/rfc/rfc2616.html#section-14.15),
[RFC 3230 §4.2](https://www.rfc-editor.org/rfc/rfc3230.html#section-4.2).

A 304 refresh keeps the stored digest fields, including their absence,
along with the rest of the metadata tied to the stored bytes. An empty
validation response can carry a Content-Digest for its own empty content.
The cache also retains its stored representation digests when the 304
supplies new values. Eligible metadata such as Cache-Control still updates.
[RFC 9530 §2](https://www.rfc-editor.org/rfc/rfc9530.html#section-2),
[RFC 9111 §3.2](https://www.rfc-editor.org/rfc/rfc9111.html#section-3.2).

Three coding tests and two cache tests failed against the previous
middleware. GHC 9.10.3 passed all 17 coding, 49 cache, 426 aggregate HTTP,
1,091 Hermes, 137 Connect, 18 WAI, 95 HTTP/1, and 125 HTTP/2 tests after
the changes. Aggregate HTTP and Connect passed under both schedules. TLS
smoke and minimal gRPC passed. Full gRPC passed 91 tests with one existing
pending case. The field-name check passed 20,553 cases including ASCII
case variants. Optimized Core compares lengths and existing folded bytes
against static literals.

Sequential Criterion measurements on the local host compared the two
middleware versions. Metadata-only decompression fixtures force the
headers without draining the body. The cache comparisons time the merge.

| Fixture | Previous middleware | Current middleware |
| --- | ---: | ---: |
| Gzip GET, ordinary metadata | 121 ns / 1,216 B | 95 ns / 960 B |
| Gzip GET, metadata with digests | 203 ns / 1,776 B | 149 ns / 960 B |
| Gzip GET with digests, drained | 1.15 µs / 39,112 B | 1.14 µs / 38,296 B |
| 304, ordinary metadata | 150 ns / 552 B | 152 ns / 552 B |
| 304, metadata with digests | 236 ns / 832 B | 196 ns / 552 B |

The checked-in policy and cache benchmarks measured 14.78 µs to drain
the full gzip GET fixture and 3.29 µs to revalidate a decoded cache entry.

## Native HTTP/2 outbound trailer validation

The native client and server use the shared HTTP/2 trailer validator
before handing outbound trailer fields to HPACK. The client checks fixed
trailers before allocating a stream. The server checks fixed trailers
before emitting the response head. For streamed bodies both senders
evaluate and validate the trailer action at EOF. Invalid streamed
request trailers cancel the request stream, and invalid response
trailers reset the response stream with INTERNAL_ERROR.
[RFC 9113 §8.1](https://www.rfc-editor.org/rfc/rfc9113.html#section-8.1),
[RFC 9113 §8.2.1](https://www.rfc-editor.org/rfc/rfc9113.html#section-8.2.1).

Both new loopback tests failed against the previous senders. The final
tests cover empty bodies, fixed bodies with content, and streamed
bodies. They verify connection reuse after failures in both directions.
The existing valid digest and repeated extension trailer round-trips
still pass. At this stage the higher-level client adapters still
dropped the trailer section.

GHC 9.10.3 passed 17 focused HTTP/2 integration, 428 aggregate HTTP,
1,091 Hermes, 137 Connect, 18 WAI, 95 HTTP/1, and 125 HTTP/2 tests.
Aggregate HTTP and Connect passed under both schedules. TLS smoke
and minimal gRPC passed. Full gRPC passed 91 tests with one existing
pending case.

Simplified Core routes the sender branches to the same field scan used
by the receivers. A focused Criterion fixture took 70 ns to validate
a Content-Digest and two extension fields. It allocated the same
32 bytes per iteration as the empty-list control. Two sequential
before/after pairs of the checked-in HTTP/2 benchmarks measured the
ordinary GET, 64 KiB POST, and streamed 64 KiB paths without trailers.
Allocation estimates shifted by less than one percent. Timing varied
between runs, especially for the POST and streaming fixtures.

## High-level HTTP client response trailers

The high-level client exposes a separate trailer section through
`rawResponseTrailers` and `responseTrailers`. Base and pooled transports
read the section after the body inside the connection scope, as does
HTTP/2 push materialisation. The streaming transport records successful
completion or a wire error alongside the bounded chunk queue. After the
caller consumes the body, the trailer action can be read repeatedly.
Cancellation can wake the worker even when the queue is full. Existing
response record constructors and protocol patterns remain available.
[RFC 9110 §6.5.2](https://www.rfc-editor.org/rfc/rfc9110.html#section-6.5.2).

Decompression drains the wire body through EOF and removes byte-dependent
digests from both headers and trailers. It edits each Trailer announcement
to keep the surviving names in order. Cache misses and live VCR recordings
return the received trailer section. Cache hits and cassette replays
discard trailers and omit their announcements. The cache entry and
cassette formats retain their existing shapes.
[RFC 9530 §6.4](https://www.rfc-editor.org/rfc/rfc9530.html#section-6.4),
[RFC 9111 §3.1](https://www.rfc-editor.org/rfc/rfc9111.html#section-3.1).

Six new regressions cover base, pooled, and streamed round-trips over
HTTP/1.1 and HTTP/2, ordered duplicate fields, digest filtering after
deflate decoding, a late HTTP/2 reset, cancellation with a one-slot body
queue, cache hits, and cassette replay. GHC 9.10.3 passed all 434 aggregate
HTTP, 1,091 Hermes, 137 Connect, 18 WAI, 95 HTTP/1, and 125 HTTP/2 tests.
Aggregate HTTP and Connect passed with synchronous and concurrent
scheduling. TLS smoke and minimal gRPC passed. Full gRPC passed 91 tests
with one existing pending case.

Core for the streaming popper reads a queued ByteString directly.
Only an empty queue reads the terminal result. On this host, a fresh
HTTP/2 connection carrying 64 KiB and two trailer fields took 506 µs
through the base transport and 559 µs through the streaming transport
with a one-slot queue. The checked-in gzip GET fixture moved from
14.71 µs and 204,143 B to 14.96 µs and 204,263 B. The partial gzip
fixture measured 54 ns and 936 B in both runs. The decoded-cache
revalidation fixture moved from 3.30 µs and 52,710 B to 3.20 µs
and 52,942 B.

## High-level HTTP client request trailers

`withRequestTrailers` adds the Trailer announcement and attaches a
producer. It validates announced names up front and validates the
produced fields at EOF. Callers can supply precomputed fields or compute
them while the body is read. The built-in base, pooled, and streamed
transports keep the producer deferred until the upload finishes.
[RFC 9110 §6.5](https://www.rfc-editor.org/rfc/rfc9110.html#section-6.5),
[RFC 9110 §6.6.2](https://www.rfc-editor.org/rfc/rfc9110.html#section-6.6.2).

Negotiated HTTP/1.1 uses chunked framing without Content-Length,
including for an empty body. HTTP/2 retains the applicable Content-Length
and sends the separate terminating HEADERS block. HTTP/1.0 rejects a
trailer-bearing request before writing its head. Body-preserving
redirects can rerun the producer, while a redirect that drops the body
also clears the announcement and producer. Request compression filters
source digests out of the header and trailer sections and their
announcement. The pool avoids retrying a deferred upload on a second
connection once it has started consuming the source.
[RFC 9112 §7.1.2](https://www.rfc-editor.org/rfc/rfc9112.html#section-7.1.2),
[RFC 9113 §8.1](https://www.rfc-editor.org/rfc/rfc9113.html#section-8.1),
[RFC 9530 §6.4](https://www.rfc-editor.org/rfc/rfc9530.html#section-6.4).

Six new integration tests cover the three transports, an empty body,
producer timing, ordered repeats, typed Content-Digest, HTTP/1.0 refusal,
an early 417 that leaves both body and trailers unread, redirects, and
deflate encoding. An independent HTTP/1.1 socket receiver checked the
exact chunked terminator and trailer bytes, including duplicate fields.
GHC 9.10.3 passed 440 aggregate HTTP, 1,091 Hermes, 137 Connect, 18 WAI,
95 HTTP/1, and 125 HTTP/2 tests. Aggregate HTTP and Connect passed with
synchronous and concurrent scheduling. TLS smoke and minimal gRPC
passed. Full gRPC passed 91 with one existing pending case.

Optimized Core returns the original request directly when it has no
trailer producer. Sequential Criterion runs on this host measured a
fresh 64 KiB upload with trailers at 211 µs and 238 KB allocated over
HTTP/1.1 and 455 µs and 486 KB over HTTP/2. The plain upload fixtures
measured 154 µs and 298 KB, and 428 µs and 538 KB respectively. The HTTP/1.1
trailer timing varied substantially. The existing 64 KiB HTTP/2 echo
fixture measured 477 µs in both binaries.

## Compact Structured Field String rendering

The shared Structured Field String renderer scans compact bytes for
quoting escapes and non-ASCII bytes. Ordinary strings write both
quotes and copy the compact payload directly into the builder buffer.
The escape path preserves the existing serialization. Signature-Input
and Accept-Signature also emit their covered components directly in
order without constructing a list of builders. Simplified Core shows
an unboxed byte scan followed by a bulk copy on the ordinary path.
[RFC 9651 §4.1](https://www.rfc-editor.org/rfc/rfc9651.html#section-4.1),
[RFC 9421 §4.1](https://www.rfc-editor.org/rfc/rfc9421.html#section-4.1).

The regression uses empty and large strings and lengths near the
builder buffer boundary, with neighboring fragments. The independent
signature and reporting matrix passed 80,292 cases. The Structured
Fields runner matched all required outcomes across 1,607 upstream
fixtures, with two permitted Base64 rejections, and passed five
supplemental byte and field-line cases.
[HTTPWG fixture corpus](https://github.com/httpwg/structured-field-tests/tree/4a4782f8234512af133f6f57c4ca998578b57110).

GHC 9.10.3 passed 1,092 Hermes, 440 aggregate HTTP, 137 Connect, 18 WAI,
95 HTTP/1, and 125 HTTP/2 tests. Aggregate HTTP and Connect passed with
both schedules. TLS and minimal gRPC passed. Full gRPC passed 91
tests with one existing pending case.

Two sequential before/after runs of the checked-in Criterion fixtures
measured the following. Go httpsfv v1.1.2 rendered the same prebuilt
Signature-Input dictionary at 320–325 ns and 552 bytes per operation.

| Fixture | Before | After |
| --- | ---: | ---: |
| Signature-Input render | 464–484 ns / 4,063 B | 188 ns / 1,040 B |
| Reporting-Endpoints render | 91–92 ns / 648 B | 80–82 ns / 312 B |

## WebDAV If rendering into a builder

The authored If renderer calculates its output size from resource,
list, and operand widths. Its writer keeps offsets strict and copies
URI and opaque entity-tag bytes into the output buffer. The typed
header method allocates a buffer of the exact size. The public builder
uses the same writer directly in its existing buffer, including when
the caller appends other fragments. Parsed values retain their wire
spelling. [RFC 4918 §10.4.2](https://www.rfc-editor.org/rfc/rfc4918.html#section-10.4.2).

The 38,200-case WebDAV corpus passed with an added check that composed
builder output agrees with typed header output on every accepted If
case. The local test also composes a tagged header containing a 4 KiB
opaque entity tag. GHC 9.10.3 passed 1,092 Hermes, 440 aggregate HTTP,
137 Connect, 18 WAI, 95 HTTP/1, and 125 HTTP/2 tests. Aggregate HTTP and
Connect passed with both schedules. TLS and minimal gRPC passed. Full
gRPC passed 91 tests with one existing pending case.

Sequential Criterion runs measured the checked-in renderer and separate
composed-builder fixtures. Simplified Core shows unboxed offsets and
direct byte-array copies. Go 1.26.10 rendered the same fixture from
nested slices in 25.7–26.6 ns and allocated 64 bytes. Its benchmark
starts with capacity for this small fixture. Hermes calculates the
required size for arbitrary trees and returns a pinned ByteString
with its backing descriptor.

| Fixture | Before | After |
| --- | ---: | ---: |
| Typed If render | 41 ns / 304 B | 38–39 ns / 240 B |
| Short If in a composed builder | 58 ns / 496 B | 46 ns / 312 B |
| 4 KiB opaque tag in a composed builder | 244 ns / 8,936 B | 134 ns / 4,640 B |

## Forwarded hop parsing and rendering

The decoder checks field-name uniqueness while it builds each hop.
Registered names use a word of seen bits and share their canonical
lowercase spellings. A case-insensitive set handles extension names.
The typed result preserves the original spelling and order. The builder
emits pairs in order and writes compact quoted values directly, including
Latin-1 wire octets. [RFC 7239 §4](https://www.rfc-editor.org/rfc/rfc7239.html#section-4).

The 131,756-case independent forwarding corpus passed. A separate
50,256-case comparison matched the previous decoder's typed values,
errors, and encoded bytes. Seventeen larger cases covered up to
4,096 extension names and quoted payloads up to 64 KiB. The focused
tests exercise mixed-case registered and extension names and name
reuse across hop boundaries. GHC 9.10.3 passed 1,092 Hermes, 440
aggregate HTTP, 137 Connect, 18 WAI, 95 HTTP/1, and 125 HTTP/2 tests.
Aggregate HTTP and Connect passed under both schedules. TLS and
minimal gRPC passed. Full gRPC passed 91 tests with one existing
pending case.

Simplified Core keeps a word bit mask on the registered-name path.
The long quote fixture contains 4,096 opaque wire octets. Sequential
Criterion runs used the checked-in parser benchmark and a separate
before/after renderer comparison. On the parser fixture Go 1.26.10
took 140–141 ns with zero heap allocation by borrowing name/value
slices and placing its results in caller-provided storage. Hermes
returns independently owned compact strings and a nested result.

| Fixture | Before | After |
| --- | ---: | ---: |
| Forwarded parse | 205–209 ns / 2,096 B | 188–195 ns / 1,360 B |
| Render registered parameters | 196 ns / 1,928 B | 57 ns / 280 B |
| Render a 4 KiB quoted extension | 36.0 µs / 509,304 B | 5.36 µs / 16,864 B |

## Compact token capture and X-Forwarded-For rendering

The shared pure parser copies successful ASCII matches directly from
the input into owned compact storage. It completes the copy while the
parser driver retains the source, and returns the same cursor and
failure outcomes. Optimized Core allocates the compact array and
copies the matched address range directly. There is no intermediate
ByteString view on this path.

X-Forwarded-For uses the existing compact comma-separated writer for
typed header output and writes compact nodes directly into a composed
builder. Both paths retain the original node spellings in field-line
order, including bracketed addresses and port suffixes.
[AWS forwarding field documentation](https://docs.aws.amazon.com/elasticloadbalancing/latest/application/x-forwarded-headers.html#x-forwarded-for).

An independent byte model and the previous implementation agreed
on 75,174 sliced-input cases, including empty captures, unaligned
offsets, and truncated input. The 131,756-case forwarding matrix
and the 1,607 HTTP Working Group Structured Fields cases passed at
their required outcomes. The latter had two permitted Base64
rejections and five supplemental cases also passed.
GHC 9.10.3 passed 1,093 Hermes, 440 aggregate HTTP, 137 Connect,
18 WAI, 95 HTTP/1, and 125 HTTP/2 tests. Aggregate HTTP and Connect
passed under both schedules. TLS and minimal gRPC passed. Full gRPC
passed 91 tests with one existing pending case.

Sequential Criterion runs measured the checked-in parse fixtures and
separate before/after renderer fixtures. X-Forwarded-For parse timings
overlapped across runs. Go 1.26.10 consumed the same input in 32–34 ns
with caller-provided storage and borrowed node slices.

| Fixture | Before | After |
| --- | ---: | ---: |
| X-Forwarded-For parse | 59–61 ns / 568 B | 54–60 ns / 496 B |
| X-Forwarded-For render, three nodes | 98 ns / 1,064 B | 25 ns / 208 B |
| X-Forwarded-For render, three nodes in a composed builder | 112 ns / 1,088 B | 33 ns / 296 B |
| X-Forwarded-For render, 4 KiB node | 297 ns / 9,568 B | 108 ns / 4,280 B |
| Accept parse | 174 ns / 1,448 B | 169 ns / 1,304 B |
| Signature-Input parse | 464 ns / 2,208 B | 471 ns / 1,992 B |
| Cookie parse | 104 ns / 760 B | 99 ns / 616 B |

## X-HTTP-Method-Override field whitespace

The typed codec trims SP and HTAB with one bounded scan from each
end and makes a single view of the remaining token. The shared method
validator checks the complete token and retains owned bytes for
extensions. Optimized Core shows unboxed indices and byte reads in both
whitespace loops. [RFC 9110 §5.6.3](https://www.rfc-editor.org/rfc/rfc9110.html#section-5.6.3)
and [§9.1](https://www.rfc-editor.org/rfc/rfc9110.html#section-9.1).

The independent octet model and the previous decoder agreed on
acceptance, emitted bytes, and errors for 1,048,765 bounded slices.
Coverage includes leading-only and trailing-only whitespace, all
interior octet pairs, all-whitespace fields, and tokens up to 32 KiB.
GHC 9.10.3 passed 1,093 Hermes, 440 aggregate HTTP, 137 Connect,
95 HTTP/1, 125 HTTP/2, and 18 WAI tests. Aggregate HTTP and Connect
passed under both schedules. TLS and minimal gRPC passed. Full gRPC
passed 91 with one existing pending case.

Three sequential before-and-after Criterion runs used the checked-in
fixtures. The matching Go 1.26.10 fixture clones the validated token.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| Extension with surrounding whitespace | 40.3–42.4 ns / 624 B | 34.0–36.3 ns / 512 B | 17.4–18.3 ns / 16 B |
| Bare extension | 26.1–26.3 ns / 424 B | 26.0–26.8 ns / 424 B | 17.1–19.3 ns / 16 B |

## X-UA-Compatible document modes and rendering

The IE mode accessor matches ASCII letters directly in the compact
tokens and checks the decimal suffix for overflow before updating the
accumulator. It retains the spelling of parsed directives. The typed
renderer sizes the complete field with checked arithmetic and copies
the engine and mode bytes into one buffer. Optimized GHC Core shows
unboxed decimal accumulation and direct byte-array copies.
[Microsoft document modes](https://learn.microsoft.com/en-us/openspecs/ie_standards/ms-iedoco/380e2488-f5eb-4457-a07a-0cb1b6e4b4b5).

The projection and rendering comparison matched previous behavior
across 344,097 cases, including mixed-case emulation prefixes, Word
boundaries, long values, and unusual bytes in authored fields.
The existing 46,450-case Refresh and X-UA-Compatible wire corpus also
passed. GHC 9.10.3 passed 1,093 Hermes, 440 aggregate HTTP, 137 Connect,
95 HTTP/1, 125 HTTP/2, and 18 WAI tests. Two HTTP/1 loopback cases timed
out on the initial concurrent aggregate run. The full concurrent
aggregate and Connect suites passed on the follow-up run. TLS and
minimal gRPC passed, and full gRPC passed 91 with one existing pending
case.

Two sequential before-and-after Criterion runs used the checked-in
fixtures. The Go 1.26.10 projection fixture returns a numeric score
and success flag. Go renders the canonical directive from a static
string. The edge projection and canonical render exceeded twice the
Go timing in this pass. See [the fixed-length follow-up](#x-ua-compatible-fixed-length-mode-checks).

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| Project edge | 38.2–38.6 ns / 288 B | 9.2–9.4 ns / 48 B | 3.2–3.6 ns / 0 B |
| Project emulation | 53.6–54.2 ns / 432 B | 12.5–12.8 ns / 48 B | 8.9–9.1 ns / 0 B |
| Project numbered mode | 41.8–42.4 ns / 432 B | 9.7–10.0 ns / 48 B | 6.3–6.6 ns / 0 B |
| Render two directives | 26.4–28.0 ns / 280 B | 22.7–23.0 ns / 208 B | 23.8–25.1 ns / 24 B |
| Render canonical edge | 5.8–5.9 ns / 32 B | 5.8 ns / 32 B | 2.6–2.7 ns / 0 B |

## X-UA-Compatible fixed-length mode checks

The projection and singleton renderer share fixed-length checks for
the two tokens in `IE=edge`. The projection sets the ASCII case bit
and the renderer compares the exact bytes before returning the cached
canonical spelling. Optimized Core reads each byte at a constant
offset and returns the cached `Just IEEdge` on the projection path.
The variant combining all byte differences with bitwise OR did not
improve the initial projection or rendering fixtures.

The public API regression exercises all 64 case combinations and
checks the emitted bytes for each. The earlier 344,097 projection and
rendering cases and 46,450 wire cases also passed. GHC 9.10.3 passed
1,093 Hermes, 440 aggregate HTTP, 137 Connect, 95 HTTP/1, 125 HTTP/2,
and 18 WAI tests. Aggregate HTTP and Connect passed under both
schedules. TLS and minimal gRPC passed. Full gRPC passed 91 with one
existing pending case.

Two sequential before-and-after Criterion runs used the same
checked-in fixtures. Go 1.26.10 used the previous projection and
rendering references, with three fresh runs. All measured projection
fixtures now take less than twice their Go time. The canonical render
still exceeds that target.

| Fixture | Previous Hermes | Current Hermes | Go |
| --- | ---: | ---: | ---: |
| Project edge | 9.0–9.6 ns / 48 B | 4.8–5.1 ns / 48 B | 3.3–3.4 ns / 0 B |
| Project emulation | 12.9–14.5 ns / 48 B | 10.9–11.3 ns / 48 B | 9.0–9.1 ns / 0 B |
| Project numbered mode | 9.5–10.5 ns / 48 B | 7.5–8.0 ns / 48 B | 6.3–6.4 ns / 0 B |
| Render canonical edge | 5.8–6.0 ns / 32 B | 6.0–6.1 ns / 32 B | 2.4–2.6 ns / 0 B |

## Connection-specific fields in the HTTP cache

Cache storage keeps the end-to-end response fields in wire order,
including repeated and unrecognized fields. It removes Connection,
its nominated fields, and the other hop-by-hop fields before
publishing the entry. A response with only chunked transfer coding
can be stored because HTTP/1 has removed the framing. The cache drops
any Content-Length that accompanied that coding. It passes through
other transfer codings and responses whose Connection options
exclude metadata needed for storing the representation.
[RFC 9111 §3.1](https://www.rfc-editor.org/rfc/rfc9111.html#section-3.1)
and [RFC 9110 §7.6.1](https://www.rfc-editor.org/rfc/rfc9110.html#section-7.6.1).

The origin response retains its original headers. Older entries
loaded from a custom store miss when they still carry hop-by-hop
fields, so request selection and freshness are recomputed. A 304
with a Connection option that removes cache metadata uses the
existing unconditional retry path.

Three new cache tests failed before the fix. The HTTP/1.1 loopback
checked that the origin used chunked framing and that a subsequent
cache hit returned the same body and end-to-end fields with one
origin request. GHC 9.10.3 passed 52 cache, 444 aggregate HTTP,
1,093 Hermes, 137 Connect, 95 HTTP/1, 125 HTTP/2, and 18 WAI tests.
Aggregate HTTP and Connect passed both schedules. TLS and minimal
gRPC passed, with 91 passing and one existing pending case in full gRPC.

Two sequential Criterion runs used a separate cache fill-and-hit
fixture and the checked-in decoded-body revalidation fixture.
The chunked fixture includes two Connection-nominated fields.
The exact chunked fast path skips the generic typed parser and
avoids a second list copy when Content-Length is absent. GHC Core
shows the general parser on the fallback path.

| Fixture | Before | After |
| --- | ---: | ---: |
| Ordinary fill and hit | 1.13–1.16 µs / 9,253 B | 1.11–1.15 µs / 9,269 B |
| Chunked fill and hit | 1.26–1.27 µs / 9,590 B | 1.45 µs / 10,089 B |
| Revalidate cached decoded body | 3.37–3.43 µs / 52,943 B | 3.27–3.37 µs / 52,775 B |

## Retiring cached responses after revalidation

A full replacement retires the selected cached response even when
response Cache-Control, Vary, or the body cap prevents storing the
replacement. This applies to foreground and background revalidation.
Server errors can leave the entry available for later validation or
stale reuse allowed by cache policy. Partial responses and unmatched
304 responses leave the stored representation available for future
validation. [RFC 9111 §4.3.3](https://www.rfc-editor.org/rfc/rfc9111.html#section-4.3.3).

A refresh, replacement, or removal checks that the cache still holds
the snapshot it originally selected. The in-memory cache does the
check and write in one STM transaction. Synchronized custom stores
perform both under their existing gate. The direct custom-store
wrapper checks the snapshot through its lookup callback before
calling the write callback. The comparison checks exchange timestamps
before comparing the full entry, as confirmed in optimized GHC Core.

Three new regressions failed before the change. Foreground tests
cover no-store, wildcard Vary, oversized bodies, and a 404. The
background test verifies eviction and draining. The concurrent
tests pause an older validation while another request stores a new
response, then complete the older request with a 304, a storable
200, or a no-store 200. A separate test covers a 503 followed by
permitted stale reuse. GHC 9.10.3 passed 56 cache, 448 aggregate HTTP,
1,093 Hermes, 137 Connect, 95 HTTP/1, 125 HTTP/2, and 18 WAI tests.
Aggregate HTTP and Connect passed under both schedules. TLS and
minimal gRPC passed, with 91 passing and one existing pending case
in full gRPC.

Two sequential before-and-after Criterion runs used a cache cycle
fixture and the checked-in decoded-body revalidation benchmark.

| Fixture | Before | After |
| --- | ---: | ---: |
| Ordinary fill and hit | 1.10–1.12 µs / 9,221 B | 1.07–1.15 µs / 9,277 B |
| Small-body 304 | 2.20–2.26 µs / 15,608 B | 2.34–2.45 µs / 15,954–15,955 B |
| 1 MiB body 304 | 55.7–56.0 µs / 3,162,341 B | 53.9–54.6 µs / 3,162,688 B |
| Revalidate cached decoded body | 3.35–3.42 µs / 52,775 B | 3.48–3.59 µs / 53,122 B |

## Freshening a cached GET from HEAD

A bodyless origin HEAD that returns 200 can update the cached GET
selected by its request fields. The cache checks each supplied ETag
and Last-Modified value, requires at least one matching validator,
and checks any supplied Content-Length against the stored field.
A candidate that fails the checks is retired. A successful refresh
applies the existing metadata merge, retaining the fields tied to
the stored body. [RFC 9111 §4.3.5](https://www.rfc-editor.org/rfc/rfc9111.html#section-4.3.5).

The cache selects the GET variant before sending HEAD and applies
the existing snapshot check on publication. A HEAD forwarded to the
origin preserves caller conditions. A request no-store directive
bypasses the metadata update.

Two new regressions failed before the change. The tests exercise
matching validators with different wire spellings, validator and
length mismatches, repeated lengths, no-store, Vary selection, and a
HEAD that finishes after a newer GET is stored. GHC 9.10.3 passed
60 cache, 452 aggregate HTTP, 1,093 Hermes, 137 Connect, 95 HTTP/1,
125 HTTP/2, and 18 WAI tests. Aggregate HTTP and Connect passed
under both schedules. TLS and minimal gRPC passed, with 91 passing
and one existing pending case in full gRPC.

Two sequential Criterion runs used fresh GET and HEAD cache cycles
and the checked-in decoded-body revalidation benchmark. The HEAD
fixture adds a GET lookup, validation, and a metadata update.
Optimized Core shows that only HEAD runs the extra lookup.

| Fixture | Before | After |
| --- | ---: | ---: |
| Ordinary fill and hit | 1.11–1.12 µs / 9,565 B | 1.13–1.21 µs / 9,637–9,638 B |
| GET, HEAD, then retained GET | 1.89–1.91 µs / 15,514 B | 3.13–3.38 µs / 20,218 B |
| Revalidate cached decoded body | 3.39–3.41 µs / 53,121 B | 3.60–3.86 µs / 53,250 B |

## Case-insensitive HTTP dates for caches

Hermes exposes an opt-in setting that folds ASCII case in weekday
names, month names, and GMT across all three HTTP-date formats.
Ordinary typed parsing uses the case-sensitive HTTP-date grammar.
Calendar validation, the RFC 850 reference clock, and canonical
rendering are shared. The HTTP cache opts in for Date, Expires,
and Last-Modified. It calculates age and expiry from the parsed
timestamps and emits a canonical If-Modified-Since on validation.
[RFC 9110 §5.6.7](https://www.rfc-editor.org/rfc/rfc9110.html#section-5.6.7)
and [RFC 9111 §4.2](https://www.rfc-editor.org/rfc/rfc9111.html#section-4.2).

The cache freshness regression failed before the change. The final
parser passed 1,080,576 casing combinations on dates covering all
months and weekdays, and 259,072 single-byte mutations against
an independent case normalization of the strict parser. All 80,616
cookie-date fixtures passed with the shared folded month matcher.
GHC 9.10.3 passed 1,094 Hermes, 61 cache, 453 aggregate HTTP,
137 Connect, 95 HTTP/1, 125 HTTP/2, and 18 WAI tests. Aggregate
HTTP and Connect passed under both schedules. TLS and minimal
gRPC passed, with 91 passing and one existing pending case in
full gRPC.

The packed comparisons use bounded loads. Keeping the weekday
and folded zone matchers separate from calendar validation reduced
code duplication in GHC Core. A like-for-like Date object build
went from 624,280 to 435,136 bytes. The generated folded-switch
candidate took 565,032 bytes.

Two sequential Criterion runs used the checked-in Date and HTTP
cache fixtures and a separate fixture for the opt-in parser.
The canonical Go reference took 75–84 ns.

| Fixture | Before | After |
| --- | ---: | ---: |
| Canonical Date parse | 103–105 ns / 936 B | 105–107 ns / 960 B |
| Canonical Date render | 25.8–27.1 ns / 160 B | 25.7–26.1 ns / 160 B |
| Mixed-case IMF-fixdate | — | 112–114 ns / 976 B |
| Mixed-case RFC 850 | — | 125–127 ns / 1,080 B |
| Revalidate cached decoded body | 3.50 µs / 53,250 B | 3.57–3.72 µs / 53,394 B |

## Retry-After and retry budgets

With `honorRetryAfter` enabled, retry middleware returns the original
response with its body unread when a valid `Retry-After` exceeds
`maxDelay`. The previous policy clamped the wait and retried early.
An accepted delay can equal the cap. Absent or malformed fields use
local backoff, as does the opt-out policy. The date calculation still
uses the live wall clock and rounds a positive wait up to the next
microsecond. [RFC 9110 §10.2.3](https://www.rfc-editor.org/rfc/rfc9110.html#section-10.2.3).

Retries and redirects use Hermes' bounded decimal parser to classify
delta-seconds without copying oversized inputs. The parser validates
the entire field before reporting a delay above budget, including all
digits and trailing field whitespace. Typed callers can still parse
and render the exact `LargeDelaySeconds` value.

Three regressions reproduced premature retries on the baseline. An
independent model agreed on 80,040 numeric scheduling cases, 20,002
absolute-date cases, and 304,640 byte-mutation and limit combinations.
GHC 9.10.3 passed 17 retry, 24 redirect, 1,095 Hermes, 455 aggregate
HTTP, 137 Connect, 18 WAI, 95 HTTP/1, and 125 HTTP/2 tests. Aggregate
HTTP and Connect passed under both schedules. TLS and minimal gRPC
passed. Full gRPC passed 91 cases with one existing pending case.

The Criterion scheduler fixture forces the result of header lookup
and delay classification on GHC 9.10.3. Optimized Core inlines the
short path and checks that eight bytes are available before each
packed load. The long-delay branch scans the input in place.

| Scheduler fixture | Before | After |
| --- | ---: | ---: |
| Absent field | 7.8 ns / 0 B | 7.7 ns / 0 B |
| One second | 26.5 ns / 136 B | 24.6 ns / 80 B |
| 3,600 seconds above budget | 26.0 ns / 120 B | 23.7 ns / 64 B |
| Valid 4,096-digit delay | 528 ns / 4,496 B | 405 ns / 304 B |
| 4,096 digits with malformed suffix | 545 ns / 4,480 B | 399 ns / 288 B |
| Absolute date | 211 ns / 1,456 B | 212 ns / 1,456 B |

The checked-in typed codec benchmarks had stable allocation at 120 B
for the four-digit value and 4,512 B for the 4,096-digit value. They
took 8.9–9.5 ns and 505–510 ns across the baseline and candidate runs.

## Caller conditions on cache hits

The private cache applies caller `If-None-Match` and `If-Modified-Since`
to reusable stored 200 responses for bodyless GET and HEAD. Cache
policy and the `Vary` selection run before evaluating the condition.
Hermes combines repeated entity-tag lines, compares opaque bytes with
weak comparison, and gives that field precedence over the date.
The date condition uses Last-Modified, then Date, then the cache's
receipt time at wire precision. Invalid or repeated date conditions
are ignored. A matching condition returns a bodyless 304 with Age,
Date, and the applicable stored validator and cache fields.
[RFC 9111 §4.3.2](https://www.rfc-editor.org/rfc/rfc9111.html#section-4.3.2)
and [RFC 9110 §15.4.5](https://www.rfc-editor.org/rfc/rfc9110.html#section-15.4.5).

Cache misses, malformed entity-tag lists, origin-only preconditions,
and ranges preserve the request field lines on forwarding. Eligible
conditional hits also work with `only-if-cached`. For stale-while-revalidate
the foreground result uses the caller's condition, and background
validation uses cache-owned validators when stripping the caller's
conditions preserves the selected variant.

Three new regressions failed on the baseline. GHC 9.10.3 passed 65
cache, 459 aggregate HTTP, 1,105 Hermes, 137 Connect, 18 WAI, 95 HTTP/1,
and 125 HTTP/2 tests. Aggregate HTTP and Connect passed under both
schedules. TLS and minimal gRPC passed. Full gRPC passed 91 tests with
one existing pending case.

Criterion compared ordinary cache hits and the checked-in decoded-body
revalidation fixture on GHC 9.10.3. The timing intervals overlapped
on both paths. Allocation increased by 48 bytes per operation in
each fixture. Optimized Core keeps the request field scan allocation
free per field and enters typed condition parsing only on a reusable
conditional response.

| Fixture | Before | After |
| --- | ---: | ---: |
| Ordinary cache hit | 435 ns / 3,520 B | 431 ns / 3,568 B |
| Revalidate cached decoded body | 4.31 µs / 53,395 B | 4.27 µs / 53,442 B |

Local conditional hits took 602 ns and 4,415 B for the weak ETag
fixture and 799 ns and 5,671 B for the date fixture. The baseline
forwarded both requests to the origin stub. The conditional timings
exclude network round-trip time.

## Preserving Vary during cache revalidation

Foreground and background cache-owned validation preserve the fields
that select the cached response. If `Vary` names `If-None-Match` and
the response has a usable ETag, the cache refetches with the original
request fields. If `Vary` names `If-Modified-Since`, the cache omits
that date condition. An available ETag can still validate the response
when its condition does not affect selection. A date-only validator
can still be used when only the tag condition would affect selection.
For stale-while-revalidate, a conditional hit can refresh in the
background when removing the caller's conditions leaves the Vary
key unchanged.
[RFC 9111 §4.1](https://www.rfc-editor.org/rfc/rfc9111.html#section-4.1)
and [§4.3.1](https://www.rfc-editor.org/rfc/rfc9111.html#section-4.3.1).

Two regressions failed on the baseline, including a foreground reply
that delivered a different representation when its request carried
an added selector. A separate matrix checked 100 combinations of
selector names and valid, missing, malformed, and repeated stored
validators. GHC 9.10.3 passed 67 cache, 461 aggregate HTTP, 1,105
Hermes, 137 Connect, 18 WAI, 95 HTTP/1, and 125 HTTP/2 tests. Aggregate
HTTP and Connect passed under both schedules. TLS and minimal gRPC
passed. Full gRPC passed 91 tests with one existing pending case.

The checked-in decoded-body revalidation benchmark had overlapping
timing intervals at 4.53 µs before and 4.52 µs after. The isolated
validator fixture took about 165 ns and 1,600 B in both ordinary runs.
With `Vary: If-Modified-Since`, the ETag-only path took 61 ns and 600 B.
With `Vary: If-None-Match`, deciding to refetch took 40 ns and 304 B.
These isolated timings exclude the origin exchange. Optimized Core
checks the selecting names before parsing an unavailable date condition.

## Cookie request ordering

The outbound `Cookie` field sends longer paths first. Cookies with equal
path lengths follow creation order, including cookies stored from separate
`Set-Cookie` lines in one response. A live replacement keeps its position.
Deletion or expiry gives a later insertion a new position. Manual and
response inserts assign order atomically with the jar update.
[RFC 6265 §5.3](https://www.rfc-editor.org/rfc/rfc6265.html#section-5.3)
and [§5.4](https://www.rfc-editor.org/rfc/rfc6265.html#section-5.4).

The jar shares the encoded Hermes name-value pair across requests. It
combines matching pairs into one field. An empty jar can pass a request
through without resolving its URI or reading the wall clock. Responses
with no applicable cookie updates leave the jar alone.

Both new order regressions failed on the baseline. An independent stable
path-length sort matched all 24 permutations of four input fields. Go's
standard jar emitted the same wire bytes for the initial, replacement,
and deletion-then-recreation cases.
[Go `cookiejar`](https://github.com/golang/go/blob/master/src/net/http/cookiejar/jar.go).

GHC 9.10.3 passed 16 cookie, 463 aggregate HTTP, 1,105 Hermes, 137
Connect, 18 WAI, 95 HTTP/1, and 125 HTTP/2 tests. Aggregate HTTP and
Connect passed under both schedules. TLS and minimal gRPC passed.
Full gRPC passed 91 tests with one existing pending case.

Criterion measured warm jars through the HTTP middleware with a local
response stub. Go 1.26.10 measured `Jar.Cookies` and `Request.AddCookie`
with a pre-parsed URL and a fresh request header map. Both fixtures
force the resulting Cookie field and exclude network time. Optimized
Core keeps URI resolution behind the nonempty-jar and response-cookie
branches. The singleton branch returns the stored field value.

| Request fixture | Previous Wireform | Current Wireform | Go 1.26.10 |
| --- | ---: | ---: | ---: |
| Empty jar | 190 ns / 1,864 B | 13.3 ns / 152 B | 90–92 ns / 48 B |
| One quoted cookie | 319 ns / 2,936 B | 240 ns / 2,352 B | 409–423 ns / 512 B |
| Four matching cookies | 492 ns / 4,048 B | 440 ns / 3,896 B | 1.47–1.51 µs / 2,528 B |

## Set-Cookie ingestion policy

The client passes its configured limits to Hermes' typed field parser.
A cookie whose `name=value` pair exceeds the jar's byte limit is
discarded. The count includes the equals sign, as in the jar's manual
insertion API. Each oversized trimmed attribute value is ignored before
it can replace an earlier valid value. The ordinary codec remains usable
for parsing and rendering fields without those jar limits.
[HTTPbis cookie draft §5.6](https://datatracker.ietf.org/doc/html/draft-ietf-httpbis-rfc6265bis-22#section-5.6).

Response and checked manual insertion share the case-insensitive name
prefix and SameSite checks. Wire `__Host-` cookies require an explicit
`Path=/`. The response path accepts `Secure` cookies from HTTPS
origins and requires `Secure` for `SameSite=None`.
[HTTPbis cookie draft §5.7](https://datatracker.ietf.org/doc/html/draft-ietf-httpbis-rfc6265bis-22#section-5.7).

Both new middleware regressions failed on the baseline. An independent
trim-and-filter model agreed on 5,915 combinations of pair limits,
attribute limits, and repeated fields, including Path, Max-Age, Expires,
flags, and retained extensions.

GHC 9.10.3 passed 18 cookie, 465 aggregate HTTP, 1,106 Hermes, 137
Connect, 18 WAI, 95 HTTP/1, and 125 HTTP/2 tests. TLS and minimal
gRPC passed. Full gRPC passed 91 tests with one existing pending case.
Two transport stress cases timed out in the first concurrent HTTP run.
The concurrent HTTP and Connect suites passed on the rerun.

The checked-in ordinary pair fixture held at about 42 ns and 416 B.
The ordinary attribute fixture went from 152 to 167 ns and held at
1,168 B. The ordinary scoped fixture went from 159 to 172 ns while
allocation fell from 1,272 to 1,256 B. The bounded parser uses a
delimiter search on the remaining input and checks attribute length
before decoding. Core confirms the delimiter search calls `memchr`.

| Parse fixture | Previous unbounded parser | Current bounded parser |
| --- | ---: | ---: |
| Ordinary scoped cookie | 176 ns / 1,232 B | 186 ns / 1,264 B |
| 8 KiB cookie value | 4.74 µs / 8,600 B | 4.79 µs / 352 B |
| 8 KiB Path value | 4.88 µs / 9,176 B | 4.40 µs / 1,160 B |

## Cookie storage scope and lifetime

The jar stores host-only and domain cookies independently, including
when their name, effective domain and path match. Updates and deletions
affect only the matching scope. The public-suffix hook accepts an
exact-host response cookie as host-only and rejects a broader domain
scope. Checked manual inserts accept host-only cookies on public
suffixes. HTTP response updates preserve overlapping, unexpired Secure
cookies. The overlap check and storage change share one STM update.
[HTTPbis cookie draft §5.7](https://datatracker.ietf.org/doc/html/draft-ietf-httpbis-rfc6265bis-22#section-5.7).

For response cookies, the jar caps both Max-Age and Expires at 400 days
from receipt. Max-Age takes precedence, including zero and negative
values that expire a cookie immediately. Session cookies and shorter
lifetimes retain their normal behavior. Hermes' typed codec preserves
the decoded wire values for callers that need to inspect or render a
field. The cap applies to response ingestion.
[HTTPbis cookie draft §5.5](https://datatracker.ietf.org/doc/html/draft-ietf-httpbis-rfc6265bis-22#section-5.5)
and [§§5.6.1–5.6.2](https://datatracker.ietf.org/doc/html/draft-ietf-httpbis-rfc6265bis-22#section-5.6.1).

The scope regressions failed on the jar before the storage change.
Commit `638867df` passed 21 cookie and 468 aggregate HTTP tests. The
400-day regression reproduced independently on that commit. It uses
both a large Max-Age and a distant absolute expiry in the same response
and checks that they are capped against the same receipt clock.

GHC 9.10.3 passed 23 cookie, 470 aggregate HTTP, 1,106 Hermes, 137
Connect, 18 WAI, 95 HTTP/1, and 125 HTTP/2 tests. Aggregate HTTP and
Connect passed under both schedules. TLS and minimal gRPC passed.
Full gRPC passed 91 tests with one existing pending case.

The Cookie receipt Criterion group measures HTTPS response ingestion
into a warm jar with a local transport stub. Optimized Core returns
the shared empty result for session cookies and calculates the
deadline only for a persistent cookie that needs it. The ordinary
Max-Age comparison uses unboxed seconds. The capped path adds whole
days to the UTC date and preserves the receipt time of day.

| Response fixture | Before | After |
| --- | ---: | ---: |
| Session | 422–434 ns / 3,840 B | 423–433 ns / 3,808 B |
| Max-Age of 3,600 seconds | 823–884 ns / 5,555 B | 828–893 ns / 5,523 B |
| Expires in year 9999 | 561–583 ns / 4,840 B | 573–606 ns / 4,864 B |

Timing ranges span the point estimates of two sequential passes with
the baseline and candidate run in reverse order on the second pass.

## HTTP signature component parameters

Signature-Input and Accept-Signature share the same component parser.
It distinguishes lower-case HTTP field names from derived names, which
use lower-case ASCII letters, digits and hyphens after the at sign.
It checks Boolean flag types, restricts field selectors to HTTP field
components, and requires a String name for `@query-param`. A `key`
selector must name a Structured Fields dictionary key. Binary wrapping
cannot be combined with strict Structured Fields serialization or
dictionary member selection on a single component.

Validation uses the final value of a repeated parameter and keeps its
original position. Extension parameters and syntactically valid extension
component names remain available to callers.
[RFC 9421 §§2.1–2.5](https://www.rfc-editor.org/rfc/rfc9421.html#section-2.1)
and [§6.4.1](https://www.rfc-editor.org/rfc/rfc9421.html#section-6.4.1).

The regression failed on the baseline. A separate model checked 30,000
inputs across the two fields, comparing acceptance and canonical bytes.
The cases cover field and derived names, parameter types and conflicts,
and duplicate parameters whose final value changes the outcome.

The empty-parameter path returns the component directly after checking
its name. Optimized Core keeps the parameter validator out of that path
and scans the name with an unboxed cursor. The checked-in Criterion
fixtures and the matching Go httpsfv v1.1.2 Dictionary and projection
fixtures produced these measurements on the same host.

| Fixture | Before | After | Go |
| --- | ---: | ---: | ---: |
| Signature-Input parse | 460–490 ns / 1,992 B | 446 ns / 1,992 B | 908–921 ns / 1,960 B |
| Accept-Signature parse | 424–445 ns / 1,888 B | 417 ns / 1,888 B | 893–906 ns / 1,904 B |

GHC 9.10.3 passed 1,107 Hermes, 470 aggregate HTTP, 137 Connect, 18
WAI, 95 HTTP/1, and 125 HTTP/2 tests. Aggregate HTTP and Connect passed
under both schedules. TLS and minimal gRPC passed. Full gRPC passed 91
tests with one existing pending case.

## Pairing message signature fields

`lookupMessageSignatures` and `lookupMessageSignaturesWithTrailers`
pair each decoded `Signature` value with its `Signature-Input` entry
by label. Results follow the metadata order, with initial field entries
before trailer entries. Each section is parsed independently. A label
missing from either dictionary or repeated across sections is an error.
Parse errors identify the field and section. An unsigned message yields
an absent result.
[RFC 9421 §4](https://www.rfc-editor.org/rfc/rfc9421.html#section-4)
and [RFC 9110 §6.5.1](https://www.rfc-editor.org/rfc/rfc9110.html#section-6.5.1).

An independent model agreed on 4,096 placements for three labels,
including absence, a header, a trailer, or both for each label in
each field. The loopback test pairs metadata from the response headers
with signature bytes from the trailers after reading the response body
over HTTP/1.1 and HTTP/2.

Optimized Core compares the labels of a single pair directly. An index
is built for multiple signatures. The Criterion fixtures use prebuilt
header maps. The Go 1.26.10 fixtures use prebuilt `http.Header` maps,
the httpsfv Dictionary parser, and a label join over both sections.

| Message fixture | Hermes | Go |
| --- | ---: | ---: |
| Unsigned | 15.9 ns / 32 B | 34.8–35.1 ns / 0 B |
| One signature pair in headers | 546 ns / 2,696 B | 1.18–1.20 µs / 2,552 B |
| One pair in each section | 847 ns / 4,647 B | 1.81–1.83 µs / 4,080 B |

GHC 9.10.3 passed 1,110 Hermes, 471 aggregate HTTP, 137 Connect, 18
WAI, 95 HTTP/1, and 125 HTTP/2 tests. HTTP and Connect passed under
both schedules. TLS and minimal gRPC passed. Full gRPC passed 91 tests
with one existing pending case.

## Authoring Content-Disposition filenames

`mkContentDispositionFilename` accepts the preferred filename as
Unicode text. Ordinary printable ASCII uses `filename`. Unicode,
backslashes, and percent-looking escapes use a UTF-8 `filename*`
parameter so the preferred spelling survives typed lookup. A supplied
printable ASCII fallback goes first. The constructor checks the
disposition token and rejects control characters and unsuitable
fallbacks.
[RFC 6266 Appendix D](https://www.rfc-editor.org/rfc/rfc6266.html#appendix-D)
and [RFC 8187 §3.2.1](https://www.rfc-editor.org/rfc/rfc8187.html#section-3.2.1).

Quoted media parameters now use the shared compact-byte writer. The
preferred-filename lookup compares parameter names directly in compact
storage. The extended-value renderer copies an all-literal value as a
span and uses a bounded writer for percent escapes. Optimized Core
threads unboxed input and output offsets through the writer.

An independent model matched 10,906 authoring and parameter-rendering
cases. The tests exercise typed field insertion and lookup, fallback
ordering, and every encoded octet across a builder buffer boundary.
GHC 9.10.3 passed 1,114 Hermes, 471 aggregate HTTP, 137 Connect, 18
WAI, 95 HTTP/1, and 125 HTTP/2 tests. HTTP and Connect passed under
both schedules. TLS and minimal gRPC passed, and full gRPC passed 91
tests with one existing pending case.

| Criterion fixture | Before | After |
| --- | ---: | ---: |
| Quoted Content-Type render | 70 ns / 712 B | 58 ns / 568 B |
| Content-Disposition render | 122 ns / 1,040 B | 110 ns / 896 B |
| Preferred extended filename lookup | 199 ns / 1,328 B | 186 ns / 1,184 B |
| Mixed extended-value render | 54 ns / 280 B | 49 ns / 280 B |
| Ordinary Content-Disposition parse | 73 ns / 552 B | 79 ns / 552 B |
| Extended Content-Disposition parse | 185 ns / 1,072 B | 176 ns / 1,088 B |

Authoring and rendering together took 202 ns and 1,960 B for the ASCII
fixture, 220 ns and 1,784 B for UTF-8, and 321 ns and 2,768 B for UTF-8
with an ASCII fallback. Go 1.26.10 `mime.FormatMediaType` with a prebuilt
parameter map took 160–163 ns and 192 B for the ASCII fixture and
251–263 ns and 320 B for the UTF-8 fixture. A separate sequential
benchmark of the extended-value writer reduced a 4 KiB literal value
from 3.31 to 2.58 µs and a 4 KiB all-escaped value from 22.1 to 11.2 µs.

## Selecting a representation with Accept

`acceptedContentQuality` rates a concrete `ContentType` against a
typed `Accept` field. Exact types outrank subtype wildcards and fully
wildcarded types. Matching parameter names break ties within the type
rank, so a more specific exclusion wins over a wildcard with positive
quality. Type, subtype, parameter names, and charset values compare
case-insensitively. Other values compare by their decoded spelling.
Repeated names count once toward specificity. The first field entry
supplies the quality when ranges are equally specific.

`preferredContentType` selects the highest positive effective quality
and keeps server order on ties. An absent field accepts any offered
type. An empty field leaves no type acceptable.
[RFC 9110 §§8.3.1–8.3.2](https://www.rfc-editor.org/rfc/rfc9110.html#section-8.3.1)
and [§§12.4.2–12.5.1](https://www.rfc-editor.org/rfc/rfc9110.html#section-12.4.2).

An independent model agreed on 5,523 requests and 49,707 effective media
qualities, including repeated field lines, exclusions, wildcards,
parameter matching, and client and server ordering. The typed
lookup tests exercise the RFC precedence example and the distinction
between absent and empty fields.

GHC 9.10.3 passed 1,118 Hermes, 471 aggregate HTTP, 137 Connect, 18
WAI, 95 HTTP/1, and 125 HTTP/2 tests. HTTP and Connect passed under
both schedules. TLS and minimal gRPC passed. Full gRPC passed 91 tests
with one existing pending case.

Criterion measured selection from parsed preferences and prebuilt
offers. The Go 1.26.10 reference uses the same ranking rules with
prebuilt structs and returns a pointer to the winning offer. Optimized
GHC Core keeps the type rank, parameter count, and weight unboxed
through the scan. An exact match with every offered parameter can
finish rating the offer immediately.

| Selection fixture | Hermes | Go |
| --- | ---: | ---: |
| Absent field | 3.10 ns / 48 B | 1.66–1.69 ns / 0 B |
| First offered type matches exactly | 12.9 ns / 48 B | 9.96–10.15 ns / 0 B |
| Wildcards and parameterized types | 103 ns / 96 B | 71.5–72.4 ns / 0 B |

## Selecting a language with Accept-Language

`acceptedLanguageQuality` rates a typed `LanguageTag` with Basic
Filtering. A range matches the whole tag or a prefix ending at a subtag
boundary, ignoring ASCII case. The longest matching range supplies the
quality, including zero. A wildcard applies when no specific range
matches. The first field entry wins ties between matching ranges of the
same length.

`preferredLanguage` shares the media selector's highest-positive-quality
scan and breaks ties in offered order. An absent field accepts any
offered tag. An empty field leaves no tag acceptable.
[RFC 4647 §3.3.1](https://www.rfc-editor.org/rfc/rfc4647.html#section-3.3.1),
[RFC 9110 §12.5.4](https://www.rfc-editor.org/rfc/rfc9110.html#section-12.5.4),
and [RFC 2616 §14.4](https://www.rfc-editor.org/rfc/rfc2616.html#section-14.4).

An independent model agreed on 7,102 requests and 99,428 language
qualities. The cases cover mixed-case subtags, repeated field lines,
exact and prefix matches, private use, grandfathered tags, wildcards,
and zero-quality exclusions. GHC 9.10.3 passed 1,122 Hermes, 471
aggregate HTTP, 137 Connect, 18 WAI, 95 HTTP/1, and 125 HTTP/2 tests.
HTTP and Connect passed under both schedules. TLS and minimal gRPC
passed. Full gRPC passed 91 tests with one existing pending case.

Criterion rates prebuilt offers against parsed fields. The Go 1.26.10
reference uses the same longest-prefix and tie rules and returns a
pointer to the selected tag. Optimized GHC Core uses a compact-byte
comparison and unboxed length and weight accumulators. The language
fixtures allocate the same amount as the absent-field selection.

| Selection fixture | Hermes | Go |
| --- | ---: | ---: |
| Absent field | 3.36 ns / 48 B | 1.67–1.70 ns / 0 B |
| First offered tag matches exactly | 9.05 ns / 48 B | 5.50–5.64 ns / 0 B |
| Prefixes, wildcard and exclusion | 57.9 ns / 48 B | 39.2–39.8 ns / 0 B |

The shared selection scan retained the media fixtures' allocation
counts. In the repeat run, media selection took 14.2 ns for the first
exact offer and 104 ns for the parameterized offers.
