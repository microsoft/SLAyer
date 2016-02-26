# About 

SLAyer is an automatic formal verification tool that uses separation logic to verify memory safety of C programs.

# Licence

SLAyer is licensed under the MIT licence included in the [LICENSE](./LICENSE) file.

# Setup

Building and testing SLAyer has some dependencies on the environment.
To set this up, start in a VS 201x shell. Then enter a bash shell, cd
to here, and execute:

`$ source config.sh`

To build SLAyer:

`$ cd src; make; cd ..`

The slayer.exe will be left in the bin directory.

See src/README for additional building instructions.

To run the tests:

Start a new VS2010+bash shell, and cd to here. (The reason to start a
new shell is that SLAyer is built using the usual VS compiler, but
when slayer runs on tests, it needs to run the WDK compiler.)

`$ source ./config.sh`
`$ cd test; source config.sh`

`$ cd sll`
`$ slayer -vSE 3 -vAbs 2 traverse.c`

See test/README for additional testing instructions.

# Contributing

To contribute, you will need to complete a Contributor License Agreement (CLA). 
Briefly, this agreement testifies that you are granting us permission to use the submitted change according to the terms of the project's license, 
and that the work being submitted is under appropriate copyright.

Please submit a Contributor License Agreement (CLA) before submitting a pull request. 
You may visit https://cla.microsoft.com to sign digitally. 
Alternatively, download the agreement Microsoft Contribution License Agreement.docx or Microsoft Contribution License Agreement.pdf), sign, scan, and email it back to cla@microsoft.com. 
Be sure to include your github user name along with the agreement. 
Once we have received the signed CLA, we'll review the request.

