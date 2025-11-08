unit buildinfo;
{$mode objfpc}{$H+}

interface

// Default development metadata (overwritten by CI during builds)
const
BUILD_NUMBER = 'dev';
BUILD_TAG    = 'dev';
BUILD_DATE   = 'local';
GIT_SHA      = 'local';
GIT_BRANCH   = 'local';
CI           = false;

implementation
end.
