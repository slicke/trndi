<?php
// Simple router for the dev server used during tests.
// - Serves existing files directly (static assets)
// - Otherwise forwards the request to testserver/index.php
// - Normalizes duplicate slashes so /api/v1//status.json works

$raw = parse_url($_SERVER['REQUEST_URI'], PHP_URL_PATH);
$path = preg_replace('#/+#', '/', $raw); // collapse multiple slashes

$docroot = __DIR__ . '/testserver';
$requested = realpath($docroot . $path);

// If requested path points to an existing file inside docroot, let the built-in server serve it
if ($requested !== false && strpos($requested, realpath($docroot)) === 0 && is_file($requested)) {
    return false; // serve the file directly
}

// Otherwise include our testserver index which will respond to API requests
chdir($docroot);
require_once __DIR__ . '/testserver/index.php';
