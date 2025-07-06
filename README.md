# Haskell Language Agnostic Unit Testing Framework for Multi-OS CI/CD Pipelines

As it stands (v0.1.0.0), this initial push is just a test runner. I will continue to develop the functionality, but only as my own needs require it. 

## Why Haskell (other than how elegant it is) ?

* Generic type derivation makes parsing a breeze
* EASY parallelization of testing with mapConcurrently, Control.Concurrent.Async (not as easy as Fortran though) 
* Haskell typing in general (strong static + safety)
    * Catches bugs at compile time to ensure structure
* OS agnostic, this is intended to be integratable into a multi-OS CI/CD pipeline which uses any test format or domain specific language 

## Functionality

Provided the tests and associated yaml/json files, this will run them in parallel then print results and total runtime. 

* _feel free to contribute to functionality_

While I have not looked into it, this base framework will make it easy to incorporate other tests formats, even those in domain specific languages.


## Dependencies
This project is built with Haskell Stack, which can be installed following this link:
https://docs.haskellstack.org/en/stable/

Upon execution of ```stack build```, the following depencies will be installed to fullfill the build:
  - base >= 4.7 && < 5
  - process
  - bytestring
  - text
  - yaml
  - aeson
  - async
  - filepath
  - time 

If this is your first time compiling Haskell, this download may take awhile
## Installation
To run this in your environment:

First compile the build:
```bash
git clone https://github.com/DOWingard/Haskell-Language-Agnostic-Unit-Testing-Framework-for-Multi-OS-CI-CD-Pipelines.git HTest
cd HTest
stack init
stack build
```
Then, install the executable so it is available in any directory:
```bash
stack install
```
## Uninstalling
If you want to remove the executable, Haskell does not have uninstall functionality, but you can locate the destination with:
```bash
stack path --local-bin
```
Copy the path it outputs, \path\to\folder, cd and remove the executable
```bash
cd "\path\to\folder"
rm htest.exe
```

## Usage
Once installed, within any project you can call 
```bash 
htest path/to/<tests.json/.yaml>
```

## Testing
There is a basic unit test set for python and C++ included in the fake_test folder which can be ran from the root folder with:

Python:
```bash
htest fake_test/python/tests.json
```
C++:
```bash
htest fake_test/cpp/tests.json
```





