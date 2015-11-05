LanguageCruxView = require './language-crux-view'
{CompositeDisposable} = require 'atom'

module.exports = LanguageCrux =
  languageCruxView: null
  modalPanel: null
  subscriptions: null

  activate: (state) ->
    @languageCruxView = new LanguageCruxView(state.languageCruxViewState)
    @modalPanel = atom.workspace.addModalPanel(item: @languageCruxView.getElement(), visible: false)

    # Events subscribed to in atom's system can be easily cleaned up with a CompositeDisposable
    @subscriptions = new CompositeDisposable

    # Register command that toggles this view
    @subscriptions.add atom.commands.add 'atom-workspace', 'language-crux:toggle': => @toggle()

  deactivate: ->
    @modalPanel.destroy()
    @subscriptions.dispose()
    @languageCruxView.destroy()

  serialize: ->
    languageCruxViewState: @languageCruxView.serialize()

  toggle: ->
    console.log 'LanguageCrux was toggled!'

    if @modalPanel.isVisible()
      @modalPanel.hide()
    else
      @modalPanel.show()
