vcl 4.1;

# Lattice CDN tier: Varnish 8 + vmod_xkey in front of the example-lattice
# demo origin. The origin speaks plain HTTP caching vocabulary
# (Cache-Control: public/s-maxage/stale-while-revalidate, ETag,
# Surrogate-Key, Vary) — this VCL only has to wire the tag-purge channel
# and keep authorized traffic out of the shared cache. See ../README.md.
#
# Varnish runs as a container (official varnish:8.0 image, which bundles
# vmod_xkey), so the backend host below is the container's alias for the
# machine running the origin: `host.containers.internal` under podman,
# `host.docker.internal` under docker. run-varnish.sh renders this file
# to a work dir first, substituting $VARNISH_BACKEND_HOST /
# $VARNISH_BACKEND_PORT when set — edit those, not this file, per run.

import xkey;

backend origin {
    .host = "host.containers.internal";
    .port = "8917";
}

# Who may invalidate. The origin's purge forwarder runs on the container
# host, and podman/docker port-publishing rewrites its source address to
# the container network's gateway — so "localhost" alone would reject
# every purge. Trust loopback plus the private/link-local ranges the
# container runtimes use. (Dev harness posture; production purgers should
# be pinned to the origin's address or carry a token.)
acl purgers {
    "127.0.0.1";
    "::1";
    "10.0.0.0"/8;
    "172.16.0.0"/12;
    "192.168.0.0"/16;
    "169.254.0.0"/16;
}

sub vcl_recv {
    # Tag purging. The origin's ocPurge forwarder issues
    #   PURGE / HTTP/1.1
    #   xkey-softpurge: reviews:Jedi Review:5009 ...
    # Soft purge expires the tagged objects but leaves grace, so the next
    # request serves stale-while-revalidate and refreshes in the
    # background. A `xkey-purge` header instead evicts outright.
    if (req.method == "PURGE") {
        if (client.ip !~ purgers) {
            return (synth(403, "Forbidden"));
        }
        if (req.http.xkey-softpurge) {
            set req.http.x-purged-count = xkey.softpurge(req.http.xkey-softpurge);
        } elsif (req.http.xkey-purge) {
            set req.http.x-purged-count = xkey.purge(req.http.xkey-purge);
        } else {
            return (synth(400, "PURGE needs an xkey-softpurge or xkey-purge header"));
        }
        return (synth(200, "Purged"));
    }

    # Mutations (POST /m/..., POST/QUERY /q) and anything else non-GET
    # go straight through — the origin handles idempotency replay itself.
    if (req.method != "GET" && req.method != "HEAD") {
        return (pass);
    }

    # Authorization-carrying requests are priv-slice traffic: never
    # shared-cached, no exceptions. (The `vc` claims parameter needs no
    # handling here — it is part of the URL and therefore of the cache
    # key, which is exactly the protocol's design.)
    if (req.http.Authorization) {
        return (pass);
    }

    # Harness debug endpoints must see the origin's true counter.
    if (req.url ~ "^/debug/") {
        return (pass);
    }

    return (hash);
}

sub vcl_backend_response {
    # vmod_xkey reads its tags from the `xkey` header on the backend
    # response; the protocol emits them as Surrogate-Key (the Fastly
    # header). Copy, don't move: Surrogate-Key stays visible downstream
    # for debugging.
    if (beresp.http.Surrogate-Key) {
        set beresp.http.xkey = beresp.http.Surrogate-Key;
    }

    # Never shared-cache private/no-store responses. The builtin VCL
    # already turns these into hit-for-miss; state it explicitly so the
    # policy survives future edits above this line.
    if (beresp.http.Cache-Control ~ "(?i)\b(private|no-store)\b") {
        set beresp.uncacheable = true;
        set beresp.ttl = 120s;
        return (deliver);
    }

    # stale-while-revalidate -> grace. VERIFIED empirically on varnishd
    # 8.0.2: RFC 5861 parsing has been native since Varnish 6.0, and a
    # fill of `s-maxage=300, stale-while-revalidate=60` logs
    # `TTL RFC 300 60 ...` (grace = 60, not the 10s default_grace). The
    # guard below is therefore a no-op on 8.0 and only exists as a
    # fallback for a varnishd that stopped mapping swr: without grace, a
    # softpurge would degrade from serve-stale-and-refresh to a plain
    # synchronous miss.
    if (beresp.http.Cache-Control ~ "stale-while-revalidate" && beresp.grace < 1s) {
        set beresp.grace = 30s;
    }
}

sub vcl_deliver {
    if (obj.hits > 0) {
        set resp.http.X-Cache = "HIT";
    } else {
        set resp.http.X-Cache = "MISS";
    }
    set resp.http.X-Cache-Hits = obj.hits;
    # The xkey working header is an implementation detail of this tier.
    unset resp.http.xkey;
}

sub vcl_synth {
    # PURGE answers with the number of invalidated objects, both as a
    # header and as a JSON body, so callers (and the harness) can assert
    # the purge really matched something.
    if (req.method == "PURGE" && req.http.x-purged-count) {
        set resp.http.xkey-purged = req.http.x-purged-count;
        set resp.http.Content-Type = "application/json";
        set resp.body = {"{"purged": "} + req.http.x-purged-count + {"}"};
        return (deliver);
    }
}
