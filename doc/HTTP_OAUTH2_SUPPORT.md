# Enhanced HTTP Support for OAuth2

## Overview

The native HTTP infrastructure has been enhanced to support OAuth2/OIDC authentication flows with:

- **Cookie jar management**: Persistent cookies across requests
- **Redirect tracking**: Automatic following of 302/301 redirects with history
- **Response headers**: Full capture of response headers including Set-Cookie
- **Status codes**: HTTP status code reporting
- **Final URL tracking**: Know where redirects ultimately landed

## New API: `requestEx`

### Signature

```pascal
function requestEx(
  const post: boolean;
  const endpoint: string;
  const params: array of string;
  const jsondata: string = '';
  cookieJar: TStringList = nil;
  followRedirects: boolean = true;
  maxRedirects: integer = 10;
  customHeaders: TStringList = nil;
  prefix: boolean = true
): THTTPResponse;
```

### THTTPResponse Record

```pascal
THTTPResponse = record
  Body: string;                  // Response body content
  Headers: TStringList;          // Response headers (key=value)
  Cookies: TStringList;          // Cookies received (Set-Cookie headers)
  StatusCode: integer;           // HTTP status code (200, 302, etc.)
  FinalURL: string;              // Final URL after redirects
  RedirectCount: integer;        // Number of redirects followed
  Success: boolean;              // True if request succeeded
  ErrorMessage: string;          // Error description if failed
end;
```

## Usage Examples

### Basic OAuth2 Flow

```pascal
var
  response: THTTPResponse;
  cookieJar: TStringList;
  authCode: string;
  i: integer;
begin
  cookieJar := TStringList.Create;
  try
    // Step 1: Login to get session cookies
    response := native.requestEx(
      true,  // POST
      'https://auth.example.com/login',
      [],
      '{"username":"user","password":"pass"}',
      cookieJar,  // Cookies will be stored here
      true,       // Follow redirects
      10          // Max 10 redirects
    );
    
    if response.Success then
    begin
      WriteLn('Login successful');
      WriteLn('Status: ', response.StatusCode);
      WriteLn('Final URL: ', response.FinalURL);
      WriteLn('Redirects: ', response.RedirectCount);
      WriteLn('Cookies: ', cookieJar.Count);
    end;
    
    // Step 2: Use cookies for subsequent request
    response := native.requestEx(
      false,  // GET
      'https://auth.example.com/authorize?client_id=xyz...',
      [],
      '',
      cookieJar,  // Reuse cookies from login
      true
    );
    
    // Step 3: Extract authorization code from redirect
    if response.RedirectCount > 0 then
    begin
      // Parse authorization code from FinalURL
      authCode := ExtractParamFromURL(response.FinalURL, 'code');
      WriteLn('Auth code: ', authCode);
    end;
    
  finally
    response.Headers.Free;
    response.Cookies.Free;
    cookieJar.Free;
  end;
end;
```

### Custom Headers Example

```pascal
var
  response: THTTPResponse;
  customHeaders: TStringList;
begin
  customHeaders := TStringList.Create;
  try
    customHeaders.Add('Authorization: Bearer token123');
    customHeaders.Add('X-Custom-Header: value');
    
    response := native.requestEx(
      false,
      'https://api.example.com/data',
      [],
      '',
      nil,
      true,
      10,
      customHeaders  // Custom headers
    );
    
    if response.Success then
      ProcessData(response.Body);
      
  finally
    customHeaders.Free;
    response.Headers.Free;
    response.Cookies.Free;
  end;
end;
```

### Redirect Analysis

```pascal
var
  response: THTTPResponse;
begin
  response := native.requestEx(
    false,
    'https://short.url/abc',
    [],
    '',
    nil,
    false  // Don't follow redirects automatically
  );
  
  try
    if response.StatusCode = 302 then
    begin
      // Extract Location header
      for i := 0 to response.Headers.Count - 1 do
      begin
        if Pos('Location:', response.Headers[i]) = 1 then
        begin
          WriteLn('Redirects to: ', 
            Copy(response.Headers[i], 11, MaxInt));
          Break;
        end;
      end;
    end;
  finally
    response.Headers.Free;
    response.Cookies.Free;
  end;
end;
```

## Platform Support

### Linux (libcURL)
Full implementation with:
- CURLOPT_COOKIELIST for cookie management
- CURLOPT_FOLLOWLOCATION for redirect handling
- CURLOPT_HEADERFUNCTION for header capture
- CURLINFO_RESPONSE_CODE for status codes
- CURLINFO_EFFECTIVE_URL for final URL tracking

### Windows/Mac
Currently falls back to basic `request()` method wrapped in THTTPResponse.
Full native implementation needed using:
- **Windows**: WinHTTP with WINHTTP_OPTION_REDIRECT_POLICY
- **Mac**: NSURLSession with cookie storage

## Integration with Tandem API

The Tandem OAuth2 implementation can now use `requestEx` to:

1. **Maintain session cookies** during login
2. **Track redirects** to extract authorization codes
3. **Capture response headers** for debugging
4. **Handle 302 redirects** in the OAuth2 flow

### Example Integration

```pascal
function Tandem.Connect: boolean;
var
  response: THTTPResponse;
  cookieJar: TStringList;
  authUrl: string;
  authCode: string;
begin
  Result := false;
  cookieJar := TStringList.Create;
  try
    // Step 1: Build authorization URL with PKCE
    authUrl := GetAuthorizationEndpoint + 
      '?client_id=' + GetOidcClientId +
      '&redirect_uri=' + GetRedirectUri +
      '&response_type=code' +
      '&scope=openid%20email%20profile' +
      '&code_challenge=' + codeChallenge +
      '&code_challenge_method=S256';
    
    // Step 2: Login with credentials
    response := native.requestEx(
      true,
      GetLoginApiUrl,
      ['username=' + FEmail, 'password=' + FPassword],
      '',
      cookieJar
    );
    
    if not response.Success then
    begin
      lastErr := 'Login failed: ' + response.ErrorMessage;
      Exit;
    end;
    
    // Step 3: Follow OAuth2 authorization flow with cookies
    response := native.requestEx(
      false,
      authUrl,
      [],
      '',
      cookieJar  // Include login cookies
    );
    
    // Step 4: Extract authorization code from final URL
    if response.RedirectCount > 0 then
    begin
      authCode := ExtractAuthCode(response.FinalURL);
      // Continue with token exchange...
    end;
    
  finally
    cookieJar.Free;
  end;
end;
```

## Notes

- Always free `THTTPResponse.Headers` and `THTTPResponse.Cookies` after use
- Cookie jar is updated automatically with received Set-Cookie headers
- Use `followRedirects := false` to manually handle redirects
- Check `response.Success` before accessing response data
- `response.StatusCode` is 0 if request failed completely

## Future Enhancements

- [ ] Implement full OAuth2 support for Windows using WinHTTP
- [ ] Implement full OAuth2 support for Mac using NSURLSession  
- [ ] Add PKCE helper functions to native base
- [ ] Add JWT parsing utilities
- [ ] Add OAuth2 token refresh helper
- [ ] Add automatic cookie jar persistence
