#!/usr/bin/env bash
#
# Shared helper for the Apptainer example runners.
#
# Apptainer performs a host-side lookup of the calling user's account and aborts
# with:
#     FATAL: Couldn't determine user account information: user: unknown userid <uid>
# when the running UID cannot be resolved. This happens, for example, on a
# CI/TeamCity agent whose UID is an LDAP/AD account that is not present in the
# local /etc/passwd and cannot be resolved through NSS.
#
# ensure_apptainer_user makes the UID resolvable again *without requiring root*,
# trying the least-intrusive method first:
#   1. Append an entry to /etc/passwd, but only when it is writable for us
#      (e.g. OpenShift-style images where /etc/passwd is group-writable, GID 0).
#   2. Otherwise redirect NSS lookups to a synthetic passwd/group via nss_wrapper
#      (needs no write access to /etc/passwd at all).
#   3. Otherwise emit a clear, actionable error.
#
# This file is meant to be sourced; it only defines a function and changes no
# global state when the user is already resolvable.

ensure_apptainer_user() {
    local uid gid user_name home_dir
    uid="$(id -u)"
    gid="$(id -g)"

    # Nothing to do when the UID can already be resolved.
    if getent passwd "$uid" >/dev/null 2>&1; then
        echo "[INFO] UID $uid is already resolvable; no Apptainer account setup needed."
        echo "##teamcity[message text='apptainer user setup: uid=$uid already resolvable, no action' status='NORMAL']"
        return 0
    fi

    user_name="${USER:-ci-user}"
    home_dir="${HOME:-/tmp}"

    echo "[INFO] UID $uid is not resolvable; setting up a temporary account entry for Apptainer."

    # 1. Preferred, rootless when /etc/passwd is writable for our process.
    if [ -w /etc/passwd ]; then
        printf '%s:x:%s:%s:%s:%s:/bin/sh\n' \
            "$user_name" "$uid" "$gid" "$user_name" "$home_dir" >>/etc/passwd
        if [ -w /etc/group ] && ! getent group "$gid" >/dev/null 2>&1; then
            printf '%s:x:%s:\n' "$user_name" "$gid" >>/etc/group
        fi
        echo "[INFO] Added account entry for $user_name (uid=$uid) to /etc/passwd."
        echo "##teamcity[message text='apptainer user setup: added /etc/passwd entry for $user_name (uid=$uid)' status='NORMAL']"
        return 0
    fi

    # 2. Rootless fallback: redirect NSS lookups via nss_wrapper.
    local wrapper_lib cand
    wrapper_lib="$(ldconfig -p 2>/dev/null | awk '/libnss_wrapper\.so/ {print $NF; exit}')"
    if [ -z "$wrapper_lib" ]; then
        for cand in /usr/lib64/libnss_wrapper.so \
                    /usr/lib/libnss_wrapper.so \
                    /usr/lib/x86_64-linux-gnu/libnss_wrapper.so; do
            if [ -e "$cand" ]; then
                wrapper_lib="$cand"
                break
            fi
        done
    fi

    if [ -n "$wrapper_lib" ]; then
        local nss_dir
        nss_dir="$(mktemp -d)"
        # Seed with the existing databases so standard accounts stay available.
        getent passwd >"$nss_dir/passwd" 2>/dev/null || :
        getent group  >"$nss_dir/group"  2>/dev/null || :
        printf '%s:x:%s:%s:%s:%s:/bin/sh\n' \
            "$user_name" "$uid" "$gid" "$user_name" "$home_dir" >>"$nss_dir/passwd"
        printf '%s:x:%s:\n' "$user_name" "$gid" >>"$nss_dir/group"

        export NSS_WRAPPER_PASSWD="$nss_dir/passwd"
        export NSS_WRAPPER_GROUP="$nss_dir/group"
        export LD_PRELOAD="$wrapper_lib${LD_PRELOAD:+:$LD_PRELOAD}"
        echo "[INFO] Using nss_wrapper ($wrapper_lib) to expose account for $user_name (uid=$uid)."
        echo "##teamcity[message text='apptainer user setup: using nss_wrapper ($wrapper_lib) for $user_name (uid=$uid)' status='WARNING']"
        return 0
    fi

    # 3. Nothing worked: surface a clear, actionable message.
    echo "[ERROR] UID $uid cannot be resolved and could not be made resolvable." >&2
    echo "[ERROR] Apptainer will abort with 'Couldn't determine user account information'." >&2
    echo "[ERROR] Remedies: run the agent as a user present in /etc/passwd, make /etc/passwd" >&2
    echo "[ERROR] writable for the agent, or install nss_wrapper (package 'nss_wrapper')." >&2
    echo "##teamcity[message text='apptainer user setup: uid=$uid could not be made resolvable' status='ERROR']"
    echo "##teamcity[buildProblem description='Apptainer: UID $uid not resolvable (cannot determine user account information)' identity='apptainerUserSetup']"
    return 1
}
