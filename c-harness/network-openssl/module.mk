# wireform-network/cbits/wireform_openssl.c: the direct-OpenSSL TLS path
# (Wireform.Network.TLS.OpenSSL). Tested end to end: in-process client and
# server over a socketpair with a self-signed EC certificate generated at
# startup through the OpenSSL API.
MODULES += network-openssl
network-openssl_SRCS := $(ROOT)/wireform-network/cbits/wireform_openssl.c
network-openssl_CFLAGS := -I$(ROOT)/wireform-network/cbits $(shell $(PKG_CONFIG) --cflags openssl)
network-openssl_LIBS := $(shell $(PKG_CONFIG) --libs openssl)
network-openssl_TESTS := test_openssl test_openssl_exit test_fail
network-openssl_FUZZERS := fuzz_ssl_accept fuzz_ssl_connect fuzz_ssl_io
# fail: failure injection (common/wf_fail.h) - the kernel's OpenSSL
# entry points are renamed to failpoint-aware fakes (failfns.h/.c) so
# test_fail can drive the construction, ALPN-copy, verify-hostname and
# I/O error-classification paths a fault BIO cannot reach.
network-openssl_VARIANTS := native fail
network-openssl_VFLAGS_fail := -DWF_FAIL_VARIANT=1 \
  -include $(COMMON)/wf_fail.h \
  -include $(HARNESS_DIR)/network-openssl/failfns.h \
  -D'SSL_CTX_new(a)=wf_fail_SSL_CTX_new(a)' \
  -D'SSL_CTX_ctrl(a,b,c,d)=wf_fail_SSL_CTX_ctrl(a,b,c,d)' \
  -D'SSL_CTX_use_PrivateKey_file(a,b,c)=wf_fail_SSL_CTX_use_PrivateKey_file(a,b,c)' \
  -D'SSL_CTX_set_ex_data(a,b,c)=wf_fail_SSL_CTX_set_ex_data(a,b,c)' \
  -D'SSL_new(a)=wf_fail_SSL_new(a)' \
  -D'SSL_set_fd(a,b)=wf_fail_SSL_set_fd(a,b)' \
  -D'SSL_get0_param(a)=wf_fail_SSL_get0_param(a)' \
  -D'X509_VERIFY_PARAM_set1_host(a,b,c)=wf_fail_X509_VERIFY_PARAM_set1_host(a,b,c)' \
  -D'CRYPTO_memdup(a,b,c,d)=wf_fail_CRYPTO_memdup(a,b,c,d)' \
  -D'CRYPTO_THREAD_run_once(a,b)=wf_fail_CRYPTO_THREAD_run_once(a,b)' \
  -D'SSL_read_ex(a,b,c,d)=wf_fail_SSL_read_ex(a,b,c,d)' \
  -D'SSL_write_ex(a,b,c,d)=wf_fail_SSL_write_ex(a,b,c,d)' \
  -D'SSL_get_error(a,b)=wf_fail_SSL_get_error(a,b)'
network-openssl_EXTRA := $(HARNESS_DIR)/network-openssl/failfns.c
# Links the uninstrumented system OpenSSL.
network-openssl_MSAN := no
