'use strict'
var _ = require('lodash')
var graphlib = require('graphlib')
var Graph = graphlib.Graph
var graphAlg = graphlib.alg

/*
 * A refract element implementation with an extensible namespace, able to
 * load other namespaces into it.
 *
 * The namespace allows you to register your own classes to be instantiated
 * when a particular refract element is encountered, and allows you to specify
 * which elements get instantiated for existing Javascript objects.
 */
function Namespace(options) {
  options || (options = {})
  return this
}

/* Graph Notes
 *
Graph = require('graphlib').Graph
graph = new Graph({directed: true})
graph.setEdge('User', 'object')
graph.setEdge('Customer', 'User')
graph.setEdge('SuperCustomer', 'Customer')
graph.setEdge('Fish', 'object')
graph.setEdge('Person', 'object')

// v: the id of the source or tail node of an edge
// w: the id of the target or head node of an edge

graph.sources() // [ 'SuperCustomer', 'Fish', 'Person' ]
graph.sinks() // [ 'object' ]
graph.predecessors('object') // [ 'User', 'Fish', 'Person' ]
graph.predecessors('Customer') // [ 'SuperCustomer' ]
graph.predecessors('User') // [ 'Customer' ]
graph.outEdges('Fish') // [ { v: 'Fish', w: 'object' } ]
graph.outEdges('SuperCustomer') // [ { v: 'SuperCustomer', w: 'Customer' } ]
graph.nodes() // [ 'User', 'object', 'Customer', 'SuperCustomer', 'Fish', 'Person' ]
graph.nodeEdges('User')
// [ { v: 'Customer', w: 'User' },
//   { v: 'User', w: 'object' } ]
graph.nodeEdges('Customer')
// [ { v: 'SuperCustomer', w: 'Customer' },
//   { v: 'Customer', w: 'User' } ]
graph.nodeEdges('SuperCustomer')
// [ { v: 'SuperCustomer', w: 'Customer' } ]
graph.nodeEdges('object')
// [ { v: 'User', w: 'object' },
//   { v: 'Fish', w: 'object' },
//   { v: 'Person', w: 'object' } ]
graph.neighbors('object') // [ 'User', 'Fish', 'Person' ]
graph.neighbors('Fish') // [ 'object' ]
graph.neighbors('User') // [ 'Customer', 'object' ]
graph.neighbors('Customer') // [ 'SuperCustomer', 'User' ]
graph.edges()
// [ { v: 'User', w: 'object' },
//   { v: 'Customer', w: 'User' },
//   { v: 'SuperCustomer', w: 'Customer' },
//   { v: 'Fish', w: 'object' },
//   { v: 'Person', w: 'object' } ]
graph.successors('object') // []
graph.successors('SuperCustomer') // [ 'Customer' ]
graph.successors('Customer') // [ 'User' ]
graph.successors('User') // [ 'object' ]
*/

Namespace.prototype.setEdge = function (parent, child) {
  var that=this
  if (_.isArray(parent) && _.isArray(child)) {
    _.forEach(parent, function(p) {
      _.forEach(child, function(c) {
        that.graph.setEdge(p, c)
      })
    })
  }
  else if (_.isArray(parent)) {
    _.forEach(parent, function(p) {
      that.graph.setEdge(p, child)
    })
  }
  else if (_.isArray(child)) {
    _.forEach(child, function(c) {
      that.graph.setEdge(parent, c)
    })
  }
  else {
    this.graph.setEdge(parent, child)
  }
}

Namespace.prototype.successor = function (node) {
  var successors = this.graph.successors(node)
  if (successors.length > 1)
    throw new VError('successor fail: %s only supposed to have single inheritance', node)

  return _.head(successors)
}

Namespace.prototype.successors = function (node) {
  return this.graph.successors(node)
}

Namespace.prototype.predecessors = function (node) {
  return this.graph.predecessors(node)
}

Namespace.prototype.topsort = function (node) {
  return graphAlg.topsort(this.graph)
}

Namespace.prototype.successorChain = function (node) {
  var sinks = this.graph.sinks() // [ 'object' ]

  if (_.isEmpty(sinks))
    throw new VError('successorChain fail: graph is empty', node)

  var sink = _.head(sinks)
  if (sink !== 'object')
    throw new VError('successorChain fail: %s expected `object` to be root', node)

  var successor = this.successor(node)
  if (!successor)
    return

  var chain = []
  while (successor !== sink) {
    chain.push(successor)
    successor = this.successor(successor)
  }
  chain.push(successor)
  return chain
}

Object.defineProperty(Namespace.prototype, 'graph', {
  get: function () {
    if (!this._graph)
      this._graph = new Graph({
        directed: true,
      })

    return this._graph
  },
  set: function (element) {
    this._baseElement = element
  },
})

// module.exports = ns
function main() {
  var ns = new Namespace()

  // module-evil.el
  // with-eval-after-loads
  // ns.setEdge('evil', ['dired', 'smartparens', 'evil-surround'])

  // Evil
  ns.setEdge('evil', [])

  // Shell
  ns.setEdge('shell', ['projectile', 'smooth-scrolling', 'magit'])
  ns.setEdge('eshell', ['auto-complete', 'helm'])

  // Buffer, Windows and Frames
  ns.setEdge('desktop', ['ignoramus'])

  // Perspective, EyeBrowse, and Helm
  ns.setEdge('perspective', ['spaceline', 'eyebrowse', 'helm'])

  // files
  ns.setEdge('file', ['ignoramus'])

  // evil packages
  ns.setEdge('evil-packages', ['magit'])

  // osx
  ns.setEdge('osx', ['dired', 'helm'])

  // markup
  ns.setEdge('markup-languages', ['flycheck'])
  ns.setEdge('latex', ['auto-complete', 'evil-packages', 'flycheck', 'flyspell',
                       'smartparens', 'typo', 'yasnippet', 'which-key'])
  ns.setEdge('markdown', ['emoji', 'smartparens', 'auto-complete', 'flyspell'])

  // programming languages
  ns.setEdge('elisp', ['auto-complete', 'help'])
  ns.setEdge('clisp', ['auto-highlight-symbol'])
  ns.setEdge('python', ['auto-complete', 'help', 'evil-packages',
                        'flycheck', 'cscope', 'semantic', 'smartparens'])
  ns.setEdge('ruby', ['smartparens', 'auto-complete', 'popwin', 'flycheck',
                      'evil-packages'])
  ns.setEdge('rust', ['flycheck', 'auto-complete', 'smartparens'])
  ns.setEdge('haskell', ['flycheck', 'auto-complete',])
  ns.setEdge('go', ['flycheck', 'auto-complete'])
  ns.setEdge('c-c++', ['auto-complete', 'flycheck', 'gtags', 'semantic',
                       'ycmd', 'cscope'])
  ns.setEdge('clojure', ['popwin', 'highlight', 'auto-complete'])
  ns.setEdge('ocaml', ['auto-complete', 'flycheck', 'smartparens'])
  ns.setEdge('purescript', ['auto-complete'])
  ns.setEdge('react', ['auto-complete', 'javascript', 'flycheck', 'web'])
  ns.setEdge('elm', ['auto-complete', 'flycheck', 'popwin', 'smartparens'])
  ns.setEdge('javascript', ['auto-complete', 'flycheck', 'evil-packages'])
  ns.setEdge('web', ['auto-complete', 'evil-packages', 'flycheck',
                     'smartparens', 'highlight', 'yasnippet'])
  ns.setEdge('lua', ['flycheck', 'auto-complete'])
  ns.setEdge('php', ['auto-complete', 'help', 'flycheck', 'gtags'])
  ns.setEdge('racket', ['auto-complete'])
  ns.setEdge('java', ['auto-complete'])
  ns.setEdge('restclient', ['auto-complete'])
  ns.setEdge('swift', ['flycheck'])
  ns.setEdge('scheme', ['auto-complete'])
  ns.setEdge('ycmd', ['auto-complete'])

  ns.setEdge('github', ['magit'])
  ns.setEdge('irc-im', ['auto-complete', 'emoji', 'perspective', 'smooth-scrolling'])
  ns.setEdge('org', ['perspective', 'evil-packages', 'auto-complete', 'emoji'])
  ns.setEdge('vinegar', ['version-control'])
  ns.setEdge('puppet', ['auto-complete', 'flycheck'])
  ns.setEdge('nixos', ['auto-complete'])
  ns.setEdge('finance', ['auto-complete'])
  ns.setEdge('flycheck', ['popwin'])
  ns.setEdge('yasnippet', ['smartparens'])
  ns.setEdge('spray-mode', ['which-key'])


  console.log(ns.topsort())
}

main()
