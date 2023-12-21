;;; config.el --- cats-javascript: Configuration

;;; Commentary:

;; Configuration for my personal javascript layer

;;; Code:

(spacemacs|define-jump-handlers js2-jsx-mode)

;; (spacemacs|define-jump-handlers rjsx-mode)

(defvar cats/javascript-mode-hook nil
  "Hooks run when javascipt type mode is fired.")

(defvar cats/javascript-yasnippets-toggle-semicolon nil
  "Whether snippets should insert semicolons as appropriate.")

(defvar cats/js-doc-author (format "%s <%s>" user-full-name user-mail-address))
(defvar cats/js-doc-url "https://gitlab.cates.io/u/andrew")
(defvar cats/js-doc-license "SEE LICENSE IN LICENSE.md")
(defvar cats/js-doc-parameter-line " * @param {} %p\n")

(defvar cats/js-doc-tags
  '(("abstract" . "This member must be implemented (or overridden) by the inheritor.")
    ("virtual" . "This member must be implemented (or overridden) by the inheritor.")
    ("access" . "Specify the access level of this member (private, public, or protected).")
    ("alias" . "Treat a member as if it had a different name.")
    ("augments" . "Indicate that a symbol inherits from, ands adds to, a parent symbol.")
    ("extends" . "Indicate that a symbol inherits from, ands adds to, a parent symbol.")
    ("author" . "Identify the author of an item.")
    ("borrows" . "This object uses something from another object.")
    ("callback" . "Document a callback function.")
    ("class" . "This function is intended to be called with the \"new\" keyword.")
    ("constructor" . "This function is intended to be called with the \"new\" keyword.")
    ("classdesc" . "Use the following text to describe the entire class.")
    ("constant" . "Document an object as a constant.")
    ("const" . "Document an object as a constant.")
    ("constructs" . "This function member will be the constructor for the previous class.")
    ("copyright" . "Document some copyright information.")
    ("default" . "Document the default value.")
    ("defaultvalue" . "Document the default value.")
    ("deprecated" . "Document that this is no longer the preferred way.")
    ("description" . "Describe a symbol.")
    ("desc" . "Describe a symbol.")
    ("enum" . "Document a collection of related properties.")
    ("event" . "Document an event.")
    ("example" . "Provide an example of how to use a documented item.")
    ("exports" . "Identify the member that is exported by a JavaScript module.")
    ("external" . "Identifies an external class, namespace, or module.")
    ("host" . "Identifies an external class, namespace, or module.")
    ("file" . "Describe a file.")
    ("fileoverview" . "Describe a file.")
    ("overview" . "Describe a file.")
    ("fires" . "Describe the events this method may fire.")
    ("emits" . "Describe the events this method may fire.")
    ("function" . "Describe a function or method.")
    ("func" . "Describe a function or method.")
    ("method" . "Describe a function or method.")
    ("global" . "Document a global object.")
    ("ignore" . "Omit a symbol from the documentation.")
    ("implements" . "This symbol implements an interface.")
    ("inheritdoc" . "Indicate that a symbol should inherit its parent's documentation.")
    ("inner" . "Document an inner object.")
    ("instance" . "Document an instance member.")
    ("interface" . "This symbol is an interface that others can implement.")
    ("kind" . "What kind of symbol is this?")
    ("lends" . "Document properties on an object literal as if they belonged to a symbol with a given name.")
    ("license" . "Identify the license that applies to this code.")
    ("listens" . "List the events that a symbol listens for.")
    ("member" . "Document a member.")
    ("var" . "Document a member.")
    ("memberof" . "This symbol belongs to a parent symbol.")
    ("mixes" . "This object mixes in all the members from another object.")
    ("mixin" . "Document a mixin object.")
    ("module" . "Document a JavaScript module.")
    ("name" . "Document the name of an object.")
    ("namespace" . "Document a namespace object.")
    ("override" . "Indicate that a symbol overrides its parent.")
    ("param" . "Document the parameter to a function.")
    ("arg" . "Document the parameter to a function.")
    ("private" . "This symbol is meant to be private.")
    ("property" . "Document a property of an object.")
    ("prop" . "Document a property of an object.")
    ("protected" . "This symbol is meant to be protected.")
    ("public" . "This symbol is meant to be public.")
    ("readonly" . "This symbol is meant to be read-only.")
    ("requires" . "This file requires a JavaScript module.")
    ("returns" . "Document the return value of a function.")
    ("return" . "Document the return value of a function.")
    ("see" . "Refer to some other documentation for more information.")
    ("since" . "When was this feature added?")
    ("static" . "Document a static member.")
    ("summary" . "A shorter version of the full description.")
    ("this" . "What does the 'this' keyword refer to here?")
    ("throws" . "Describe what errors could be thrown.")
    ("exception" . "Describe what errors could be thrown.")
    ("todo" . "Document tasks to be completed.")
    ("tutorial" . "Insert a link to an included tutorial file.")
    ("type" . "Document the type of an object.")
    ("typedef" . "Document a custom type.")
    ("variation" . "Distinguish different objects with the same name.")
    ("version" . "Documents the version number of an item.")))

(defvar cats/js2-global-externs
  '("__dirname"
    "_"
    "describe"
    "it"
    "before"
    "after"
    "beforeEach"
    "afterEach"
    "chai"
    "sinon"
    "expect"
    ))

;;; config.el ends here
