# Tandem Source API Implementation Notes

## Overview
A Tandem Source API driver has been created in `trndi.api.tandem.pp` based on the tconnectsync v2 project. The basic structure is in place, but full implementation requires additional HTTP infrastructure support.

## Current Status

### Completed
- ✅ Basic API driver structure following Trndi patterns
- ✅ Region-specific implementations (US and EU)
- ✅ Integration into configuration UI (uconf)
- ✅ Integration into main application (umain)
- ✅ Test connection framework

### Requires Additional Work

#### 1. OAuth2/OIDC Authentication Flow
The Tandem Source API uses a complex OAuth2 with PKCE flow that requires:
- **Session/Cookie Management**: The native HTTP wrapper needs to maintain cookies across requests
- **Redirect Handling**: Need to follow 302 redirects and extract authorization codes from callback URLs
- **State Management**: PKCE requires generating and storing code verifiers between requests

**Recommendation**: Consider extending `TTrndiNativeBase.request()` to:
```pascal
function requestWithCookies(const post: boolean; const endpoint: string;
  const params: array of string; const jsondata: string = '';
  const header: string = ''; followRedirects: boolean = true;
  var cookies: TCookieJar): string;
```

#### 2. JWT Validation
The current implementation does a simplified JWT extraction without proper signature validation. For production:
- Parse JWKS from the JWKS endpoint
- Validate JWT signature using RSA public keys
- Verify issuer, audience, and expiration claims

**Dependencies**: This requires a proper JWT library or implementing RS256 signature verification.

#### 3. Pump Event Parsing
The `GetReadings` method needs refinement to properly parse Tandem's pump event format:
- Events are stored in a complex nested JSON structure
- CGM readings are in `eventData` arrays within event objects
- Need to handle multiple event types (EGV, CGM, Basal, Bolus)
- Timestamps are in ISO 8601 format and may need timezone handling

#### 4. Testing Requirements
- The implementation needs testing against real Tandem accounts (both US and EU)
- Mock server for unit tests
- Handle various edge cases (expired tokens, missing devices, etc.)

## Simplified Alternative Approach

If the OAuth2 complexity is too much, consider a hybrid approach:
1. User logs in via the Tandem mobile app or web interface
2. User manually copies their access token from browser developer tools
3. Trndi stores and uses the token until it expires
4. On expiration, user needs to manually refresh

This would simplify the implementation significantly while still providing value.

## Files Modified

### New Files
- `units/trndi/api/trndi.api.tandem.pp` - Main Tandem API driver

### Modified Files
- `units/forms/uconf.pp` - Added Tandem to configuration UI
- `units/forms/uconf.lfm` - Added Tandem options to dropdown
- `units/forms/umain.pp` - Added Tandem uses clause
- `inc/umain_init.inc` - Added Tandem initialization cases
- `Trndi.lpr` - Added Tandem unit to project

## Next Steps

1. **Implement Cookie/Session Support** in native HTTP wrapper
2. **Test OAuth2 Flow** against Tandem's servers
3. **Refine Pump Event Parsing** based on real API responses
4. **Add Proper Error Handling** for authentication failures
5. **Implement Token Refresh** logic
6. **Add JWT Signature Validation** for security
7. **Create Unit Tests** with mock server

## References
- tconnectsync v2: https://github.com/jwoglom/tconnectsync
- Tandem Source API documentation (if available from Tandem)
- OAuth 2.0 with PKCE: RFC 7636
- OpenID Connect specification

## Contact
For questions about this implementation, refer to the tconnectsync project or Tandem's developer documentation.
