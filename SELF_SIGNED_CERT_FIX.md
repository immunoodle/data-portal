# Authentication Fixes: SSL Certificate Issues & Local Dev Mode

## Problems Fixed

### 1. SSL Certificate Verification Issues
When connecting to a Dex instance with SSL/TLS, authentication was failing with certificate verification errors. This can happen with:
- **Self-signed certificates** (common in dev/staging)
- **Properly signed certificates with missing intermediate certs** (like InCommon CA)
- **System trust store issues** where the CA isn't in R's certificate bundle

### 2. Local Dev Mode Errors  
When running in `LOCAL_DEV=1` mode, the app would error trying to fetch OIDC discovery endpoints even though authentication was being bypassed.

## Background: Your Dex Certificate

Your Dex server uses:
- **Certificate**: `*.dartmouth.edu`
- **Issuer**: InCommon RSA Server CA 2 (via Internet2)
- **Valid**: June 11, 2025 - June 12, 2026

This is a **properly signed certificate** from a trusted educational CA. However, httr2/curl may not have InCommon's CA in its trust store, causing verification failures.

## Solutions

### 1. SSL Certificate Fix
Updated all HTTPS requests to Dex endpoints to support custom CA certificates/bundles. You can provide:
- The InCommon CA bundle (for your case)
- A self-signed certificate (for dev environments)
- The full certificate chain

### 2. Local Dev Mode Protection
Added checks to skip OIDC configuration entirely when running in local development mode.

## Changes Made

### 1. auth_config.R
- Added new environment variable: `DEX_CA_CERT_PATH`
- Added `IS_LOCAL_DEV` flag detection
- Wrapped all OIDC discovery and configuration in dev mode check
- Updated `get_oidc_discovery()` to accept optional `ca_cert_path` parameter
- Updated `get_jwks()` to accept optional `ca_cert_path` parameter  
- Updated `test_oidc_discovery()` to accept optional `ca_cert_path` parameter
- All functions now use `httr2::req_options(cainfo = ca_cert_path)` when a valid cert path is provided
- Skip discovery test in dev mode

### 2. app.R
- Updated `exchange_code_for_token()` to add CA cert support for token endpoint requests
- Updated the JWKS refetch call to pass `DEX_CA_CERT_PATH`
- Login button already had dev mode protection (no changes needed)

## Configuration

### SSL Certificate Setup

**When do you need to provide a certificate?**

You need `DEX_CA_CERT_PATH` if you see SSL verification errors, which can happen with:

1. **Self-signed certificates** (dev/staging environments)
   - Get the self-signed cert from your infra team
   
2. **Trusted but not recognized CAs** (like InCommon for `*.dartmouth.edu`)
   - Your Dex uses InCommon, which is trusted but may not be in httr2's bundle
   - Download the InCommon CA bundle
   
3. **Missing intermediate certificates**
   - Server doesn't send the full certificate chain
   - Provide the full chain file

**When you DON'T need this:**
- ❌ If Dex uses a widely recognized CA (Let's Encrypt, DigiCert, etc.)
- ❌ If running Dex locally with `http://` (no SSL)
- ❌ If you're not getting SSL errors

**Getting the InCommon CA Bundle (for your Dartmouth Dex):**

```bash
# Download InCommon CA bundle
curl -o incommon-ca-bundle.pem https://www.incommon.org/custom/certificates/repository/sha384%20Intermediate%20(txt).txt

# Or get from your infra team
```

**Setup:**

```bash
# Path to the CA certificate bundle
DEX_CA_CERT_PATH=/path/to/incommon-ca-bundle.pem
# or
DEX_CA_CERT_PATH=/path/to/your-self-signed-cert.pem
```

### Local Development Mode

To run without Dex authentication (useful for local development):

```bash
LOCAL_DEV=1
```

When enabled:
- All OIDC discovery and configuration is skipped
- Authentication is bypassed with dummy user credentials
- Login button sets `dev_user@dartmouth.edu` as the authenticated user
- No Dex endpoints are required

##```

### Environment Variables Summary

```bash
# Required for production with Dex authentication
DEX_ISSUER=https://your-dex-server.dartmouth.edu
DEX_CLIENT_ID=your-client-id
DEX_CLIENT_SECRET=your-client-secret

# Optional - provide if you get SSL verification errors
# For Dartmouth's InCommon-signed Dex:
DEX_CA_CERT_PATH=/app/certs/incommon-ca-bundle.pem

# Optional - for local development without Dex
LOCAL_DEV=1
```

## Why Did This Break?

Your old version was working with Dex on HTTPS. This likely broke because:

1. **Certificate renewal** - New cert issued June 2025 with different properties
2. **R/httr2 update** - Newer version has stricter SSL validation or different CA bundle
3. **System changes** - Docker base image update that changed the system trust store
4. **Missing intermediate cert** - New Dex configuration not sending InCommon intermediate CA

The fix allows you to explicitly provide the CA bundle, bypassing the system trust store issues.

## Quick Fix for Dartmouth Dex

Since your Dex uses InCommon (a known CA), you have two options:

### Option 1: Provide InCommon CA Bundle (Recommended)
```bash
# Download InCommon bundle
curl -o /path/to/incommon-ca-bundle.pem \
  https://www.incommon.org/custom/certificates/repository/sha384%20Intermediate%20(txt).txt

# Set in environment
export DEX_CA_CERT_PATH=/path/to/incommon-ca-bundle.pem
```

### Option 2: Ask Infra Team
Ask your infra team to:
- Ensure Dex sends the complete certificate chain (including InCommon intermediate CA)
- Verify the InCommon CA is in their system trust store
- Provide you with the CA bundle they're using

### Option 3: Disable SSL Verification (NOT RECOMMENDED for production)
Only for testing, you could add an option to disable SSL verification, but this is a security risk.

### Docker Setup
If running in Docker, make sure to:
1. Mount the certificate file into the container
2. Set the `DEX_CA_CERT_PATH` to the container path

Example docker-compose.yml:
```yaml
volumes:
  - /host/path/to/ca-cert.pem:/app/certs/ca-cert.pem:ro
environment:
  - DEX_CA_CERT_PATH=/app/certs/ca-cert.pem
  - DEX_ISSUER=https://your-dex-server.com
  - DEX_CLIENT_ID=your-client-id
  - DEX_CLIENT_SECRET=your-client-secret
  # Omit for production, set to 1 for local dev
  # - LOCAL_DEV=1
```

## Backward Compatibility
These changes are fully backward compatible:
- If `DEX_CA_CERT_PATH` is not set or is empty, the code works as before
- If the path is set but the file doesn't exist, it falls back to default behavior
- No changes needed for Dex instances with properly signed certificates
- `LOCAL_DEV` defaults to `0` (disabled) if not set

## Testing

### Production Mode (with Dex)
Test the OIDC discovery on startup. You should see:
```
Testing OIDC discovery...
Using custom CA certificate: /path/to/ca-cert.pem  # Only if using self-signed cert
✅ OIDC Discovery successful
✅ Authentication configuration loaded successfully.
```

### Local Dev Mode
When `LOCAL_DEV=1`, you should see:
```
⚠️  LOCAL_DEV mode enabled - Skipping OIDC configuration
Authentication will be bypassed with dummy user credentials
```

If the certificate is invalid or path is wrong, you'll see connection errors in the logs.

