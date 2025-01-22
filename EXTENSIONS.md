# Trndi Extensions
> This list is currently __incomplete__!
Trndi supports extensions, written in JavaScript.

You can declare functions inside Trndi and expose them to JS, or "listen" for functions in Trndi.

Trndi supports promises and async code! See [the guide](guides/Extensions.md) for information on how to install them.

## Basics
#### alert("Alert-test!");
Shows a pop-up
> This triggers ```JsDoAlert()``` in _trndi.ext.jsbase.inc_ 
#### console.log("log-test")
Creates a log popup.
> This triggers ```JSConsoleLog()``` in _trndi.ext.functions.pp_ 
}

### Advanced
#### uxProp
Modifies the Trndi UX

## Promises
#### bgDump
Directly inputs a query to the backend, to get recent readings. (min: int, max: int)
> This triggers ``` TJSFuncs.bgDump()``` in _trndi.ext.jsfuncs.pp_ 
}

#### asyncGet
Fetches a URL and returns the data. (url: string)
> This triggers ``` TJSFuncs.asyncGet()``` in _trndi.ext.jsfuncs.pp_ 
}

#### querySvc
Allows for a direct, unfiltered, query to the backend service

## Callbacks
#### uxCallback
Triggers when a uxProp has completed.
> Returns __array__: [component, value:_string_]

## Demo
__NOTE:__ There is full ES2023 language support.

Trndi creates the _TrndiExtension_ object.
```
// Get a file and display the result in the GUI
asyncGet("https://sample-files.com/downloads/documents/txt/simple.txt")
  .then(result => console.log(result))
  .catch(error => console.log(`Error: ${error}`));

// Display a GUI update
 function uxCallback(a,b){
 alert(`Changed: ${a} with ${b}`);
}

// Change element colors
function color(){
  // Change a UX property of the main form
  uxProp('ldiff', 'font-color', '#000000');
  uxProp('ldiff', 'font', 'Comic Sans');
}

color();
```

### Run something
This is a theoretical command, I havent tested it:
```
runCMD("app", `-v;--url;"smtps://smtp.gmail.com:465";--mail-from;you@gmail.com";--mail-rcpt;"another@stuff.com";--mail-data;"Email";--user;"you@gmail.com:APP-PASSWORD"`, ";").then(result => console.log(result))
  .catch(error => console.log(`Error: ${error}`));
  ```
  As you see spaces are replaced with ";" to allow spaces inside params. The last parameter actually defines where to split the paraneters. you can use `${X}" to pass data, if calling from a callback eg