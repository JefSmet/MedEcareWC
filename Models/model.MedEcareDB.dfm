object MedEcareDB: TMedEcareDB
  Height = 429
  Width = 658
  object reqGetActivities: TWebHttpRequest
    Headers.Strings = (
      'Content-Type:application/json')
    ResponseType = rtJSON
    Left = 248
    Top = 24
  end
  object reqPostVerlof: TWebHttpRequest
    Command = httpPOST
    Headers.Strings = (
      'Content-Type:application/json')
    ResponseType = rtText
    Left = 232
    Top = 240
  end
  object reqPutVerlof: TWebHttpRequest
    Command = httpPUT
    Headers.Strings = (
      'Content-Type=application/json')
    ResponseType = rtText
    Left = 352
    Top = 240
  end
end
