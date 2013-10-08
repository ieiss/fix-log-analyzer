% ==================================================================================
% Copyright (c) 2011 | IEISS | www.ieiss.com | info@ieiss.com
% 
% Permission is hereby granted, free of charge, to any person obtaining
% a copy of this software and associated documentation files (the "Software"),
% to deal in the Software without restriction, including without limitation
% the rights to use, copy, modify, merge, publish, distribute, sublicense,
% and/or sell copies of the Software, and to permit persons to whom the Software
% is furnished to do so, subject to the following conditions:
%
% The above copyright notice and this permission notice shall be included in
% all copies or substantial portions of the Software.
%
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
% IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
% CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
% TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
% OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
% ==================================================================================

% Source Code Folder Structure
% ----------------------------
%    ../src
%    ../ebin

% Source Code File Description
% ----------------------------
% ../src/log_analyzer.erl       : Source code logic.
% ../src/log_analyzer_tests.erl : Source code logic.
% ../src/utils.erl              : Source code logic.
% ../src/make-script            : A basic script to compile source files and place under ../ebin directory
% ../ebin/config.txt            : A sample configuration file defining FIX message types
% ../ebin/fixlog.bin            : A sample binary stream of FIX messages (provided for testing purpose)

% Executing Program
% ----------------------------
% Running ../src/make-script will place binaries under ../ebin
% Start Erlang interpreter under ../ebin, and execute log_analyzer_tests:t001_test_().
% The program reads FIX Message definitions from "config.txt" and parses the binary data "fixlog.bin" accordingly
% Output file will be saved under name output***.txt 


% NOTE
% ----------------------------
% /1/ The "config.txt" file that comes with this program matches the tag/value pair definition on which "fixlog.bin" is based.
% /2/ You would need to change "config.txt" to match your own FIX system
