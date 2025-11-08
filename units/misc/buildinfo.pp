unit buildinfo;
{$mode objfpc}{$H+}

interface

// Default development metadata (overwritten by CI during builds)
const
BUILD_NUMBER = 'dev';
BUILD_TAG    = 'dev';
BUILD_DATE   = '2024-06-01T00:00:00Z';
GIT_SHA      = 'local';
GIT_BRANCH   = 'local';
CI           = false;

implementation
end.
