# License

*LmcpGen* is developed by the Air Force Research Laboratory, Aerospace System Directorate, Power and Control Division. 
The LMCP specification and all source code for *LmcpGen* is publicaly released under the Air Force Open Source Agreement
Version 1.0. See LICENSE.md for complete details. The Air Force Open Source Agreement closely follows the NASA Open Source
Agreement Verion 1.3. **NOTE the terms of the license include registering use of the software by emailing <a href="mailto:afrl.rq.opensource@us.af.mil?subject=LmcpGen Registration&body=Please register me for use of LmcpGen. Name: ____________">afrl.rq.opensource@us.af.mil</a>.**

# Introduction

The Lightweight Message Construction Protocol (LMCP) is a standard that serves two purposes. First, it
defines a structure for common structured data and a process for serializing objects based on those
types. Secondly, it defines a method for encapsulating objects for transmission between applications.
This specification describes the structure of data, without specifying how applications implement the
handling instantiations of those data types. Applications that implement this specification can send
and receive objects regardless of the operating system, platform, or programming language used. LMCP
is a simple and extensible specification, so it can be implemented without central runtime, proprietary
libraries, or the complexity of other protocols such as HLA and DIS. This system offers several
advantages to the end-user, including:

- A design independent of language, platform, and transport protocol.
- Object-oriented approach through structured data types, including support for null objects.
- A high level of customizability through user-defined data models.
- Support for variable length arrays and nested objects.
- Simple and open design, free of proprietary code and requiring no runtime infrastructure or special libraries.

LMCP also defines the structure of a message. A message is an LMCP object that is encapsulated with header
and footer items to enable communication of LMCP objects between applications. LMCP allows developers to
create custom data types (structs) easily through a message data model (MDM). Custom classes can be created
for a given MDM through automatic code generation or other means to recognize and handle messages.

LMCP was created at the Air Force Research Library as a way to ease communications between applications written
in a variety of computing languages and running on different computing platforms. For more information on
LMCP and its precise specification, see the *doc* directory in the *LmcpGen* git repository.

# Using *LmcpGen*

The *LmcpGen* tool autogenerates source-code libraries in several languages that conform to the serialization
specification of LMCP. Currently, *LmcpGen* creates libraries for the following languages:

- Java
- C++
- C#
- Python

Additionally, *LmcpGen* can create HTML documentation in the form of an easy-to-navigate webpage for viewing the
messages described in source MDMs.

## Requirements

To use *LmcpGen*, a Java JRE version 1.8 or higher is required. To modify *LmcpGen*, the Java JDK 1.8 or higher
is required. *LmcpGen* does not rely on any external libraries and is completely stand-alone. For convenience,
Netbeans project files are included to allow developers a quick way to change and re-build *LmcpGen*.

## Installation

*LmcpGen* is a simple Java program that will run on any system with [Java][java download] installed.

[java download]: https://java.com/en/download/

## Running *LmcpGen*

*LmcpGen* can be run both in a user-interactive mode or silently as a command line tool. To run in user-interactive
mode, double-click *LmcpGen.jar* or use the command

``` java -jar LmcpGen.jar ```

If *LmcpGen* is run with any additional command-line arguments, then it will run silently until completion. The
command line options are as follows:
 - `-mdm <filename>` path to the MDM XML file.  Multiple MDMs can be specified by repeating the -mdm tag.
 - `-mdmdir <directory path>` to directory containing multiple MDM XML files. Can be used multiple times to specify multiple directories.
 - `-java` Adds proper template and method name for Java output.
 - `-cpp` Adds proper template and method name for C++ output. Note: creates c++11 compatible code.
 - `-cs` Adds proper template and method name for C# output.
 - `-py` Adds proper template and method name for Python output.
 - `-xsd` Adds proper template and method name for XML schema output.
 - `-doc` Adds proper template and method name for documentation output.
 - `-dir <directory path>` path to the directory where files are to be written.
 - `-checkMDM <mdm file>` Checks the MDM file for errors and exits.

For example, with a set of proper LMCP MDMs in the directory `mdms`, the command to create documentation would be:

``` java -jar LmcpGen.jar -mdmdir "mdms" -doc -dir "doc/LMCP" ```
