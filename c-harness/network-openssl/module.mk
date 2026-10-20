# wireform-network/cbits/wireform_openssl.c: the direct-OpenSSL TLS path
# (Wireform.Network.TLS.OpenSSL). Tested end to end: in-process client and
# server over a socketpair with a self-signed EC certificate generated at
# startup through the OpenSSL API.
MODULES += network-openssl
network-openssl_SRCS := $(ROOT)/wireform-network/cbits/wireform_openssl.c
network-openssl_CFLAGS := -I$(ROOT)/wireform-network/cbits $(shell $(PKG_CONFIG) --cflags openssl)
network-openssl_LIBS := $(shell $(PKG_CONFIG) --libs openssl)
network-openssl_TESTS := test_openssl
network-openssl_FUZZERS := fuzz_ssl_accept fuzz_ssl_connect fuzz_ssl_io
# Links the uninstrumented system OpenSSL.
network-openssl_MSAN := no
