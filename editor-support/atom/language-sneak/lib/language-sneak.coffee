LanguageSneakView = require './language-sneak-view'
{CompositeDisposable} = require 'atom'

module.exports = LanguageSneak =
  languageSneakView: null
  modalPanel: null
  subscriptions: null

  activate: (state) ->
    @languageSneakView = new LanguageSneakView(state.languageSneakViewState)
    @modalPanel = atom.workspace.addModalPanel(item: @languageSneakView.getElement(), visible: false)

    # Events subscribed to in atom's system can be easily cleaned up with a CompositeDisposable
    @subscriptions = new CompositeDisposable

    # Register command that toggles this view
    @subscriptions.add atom.commands.add 'atom-workspace', 'language-sneak:toggle': => @toggle()

  deactivate: ->
    @modalPanel.destroy()
    @subscriptions.dispose()
    @languageSneakView.destroy()

  serialize: ->
    languageSneakViewState: @languageSneakView.serialize()

  toggle: ->
    console.log 'LanguageSneak was toggled!'

    if @modalPanel.isVisible()
      @modalPanel.hide()
    else
      @modalPanel.show()
