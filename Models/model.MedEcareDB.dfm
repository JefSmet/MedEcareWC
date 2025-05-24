object MedEcareDB: TMedEcareDB
  Height = 429
  Width = 658
  object reqGetActivities: TWebHttpRequest
    Headers.Strings = (
      'Content-Type:application/json')
    ResponseType = rtText
    Left = 120
    Top = 240
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
  object reqDeleteVerlof: TWebHttpRequest
    Command = httpDELETE
    Headers.Strings = (
      'Content-Type=application/json')
    ResponseType = rtText
    Left = 448
    Top = 240
  end
end
