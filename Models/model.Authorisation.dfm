object Authorisation: TAuthorisation
  Height = 425
  Width = 617
  object WebSessionStorage1: TWebSessionStorage
    Left = 64
    Top = 40
  end
  object WebLocalStorage1: TWebLocalStorage
    Left = 64
    Top = 136
  end
  object WebHttpRequest1: TWebHttpRequest
    Headers.Strings = (
      'Content-Type:application/json')
    URL = 'http://localhost:3000'
    Left = 224
    Top = 40
  end
end
