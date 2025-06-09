object MedEcareDB: TMedEcareDB
  Height = 833
  Width = 946
  object reqGetActivities: TWebHttpRequest
    Headers.Strings = (
      'Content-Type:application/json')
    ResponseType = rtText
    Left = 120
    Top = 48
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
  object reqGetStaff: TWebHttpRequest
    Headers.Strings = (
      'Content-Type=application/json')
    ResponseType = rtText
    Left = 120
    Top = 136
  end
  object reqGetShiftsByPeriod: TWebHttpRequest
    Headers.Strings = (
      'Content-Type=application/json')
    ResponseType = rtText
    Left = 248
    Top = 136
  end
  object reqGetVerlofByPeriod: TWebHttpRequest
    Headers.Strings = (
      'Content-Type=application/json')
    ResponseType = rtText
    Left = 392
    Top = 136
  end
  object reqGetShiftTypes: TWebHttpRequest
    Headers.Strings = (
      'Content-Type:application/json')
    ResponseType = rtText
    Left = 112
    Top = 328
  end
  object reqPostShiftTypes: TWebHttpRequest
    Command = httpPOST
    Headers.Strings = (
      'Content-Type:application/json')
    ResponseType = rtText
    Left = 224
    Top = 336
  end
  object reqPutShiftTypes: TWebHttpRequest
    Command = httpPUT
    Headers.Strings = (
      'Content-Type=application/json')
    ResponseType = rtText
    Left = 344
    Top = 336
  end
  object reqDeleteShiftTypes: TWebHttpRequest
    Command = httpDELETE
    Headers.Strings = (
      'Content-Type=application/json')
    ResponseType = rtText
    Left = 448
    Top = 336
  end
  object reqGetRoles: TWebHttpRequest
    Headers.Strings = (
      'Content-Type:application/json')
    ResponseType = rtText
    Left = 112
    Top = 424
  end
  object reqPostRoles: TWebHttpRequest
    Command = httpPOST
    Headers.Strings = (
      'Content-Type:application/json')
    ResponseType = rtText
    Left = 224
    Top = 424
  end
  object ReqPutRoles: TWebHttpRequest
    Command = httpPUT
    Headers.Strings = (
      'Content-Type=application/json')
    ResponseType = rtText
    Left = 336
    Top = 424
  end
  object reqDeleteRoles: TWebHttpRequest
    Command = httpDELETE
    Headers.Strings = (
      'Content-Type=application/json')
    ResponseType = rtText
    Left = 448
    Top = 424
  end
  object reqPostActivites: TWebHttpRequest
    Command = httpPOST
    Headers.Strings = (
      'Content-Type:application/json')
    ResponseType = rtText
    Left = 232
    Top = 48
  end
  object reqPutActivities: TWebHttpRequest
    Command = httpPUT
    Headers.Strings = (
      'Content-Type=application/json')
    ResponseType = rtText
    Left = 344
    Top = 48
  end
  object reqDeleteActivities: TWebHttpRequest
    Command = httpDELETE
    Headers.Strings = (
      'Content-Type=application/json')
    ResponseType = rtText
    Left = 464
    Top = 48
  end
  object reqDeletePersons: TWebHttpRequest
    Command = httpDELETE
    Headers.Strings = (
      'Content-Type=application/json')
    ResponseType = rtText
    Left = 448
    Top = 496
  end
  object reqPutPersons: TWebHttpRequest
    Command = httpPUT
    Headers.Strings = (
      'Content-Type=application/json')
    ResponseType = rtText
    Left = 336
    Top = 496
  end
  object reqPostPersons: TWebHttpRequest
    Command = httpPOST
    Headers.Strings = (
      'Content-Type:application/json')
    ResponseType = rtText
    Left = 224
    Top = 496
  end
  object reqGetPersons: TWebHttpRequest
    Headers.Strings = (
      'Content-Type:application/json')
    ResponseType = rtText
    Left = 112
    Top = 496
  end
  object reqPutUser: TWebHttpRequest
    Command = httpPUT
    Headers.Strings = (
      'Content-Type=application/json')
    ResponseType = rtText
    Left = 112
    Top = 576
  end
  object reqPutChangePassword: TWebHttpRequest
    Command = httpPUT
    Headers.Strings = (
      'Content-Type=application/json')
    ResponseType = rtText
    Left = 224
    Top = 576
  end
  object reqGetRoster: TWebHttpRequest
    Headers.Strings = (
      'Content-Type:application/json')
    ResponseType = rtText
    Left = 112
    Top = 664
  end
  object reqGetShiftTypeById: TWebHttpRequest
    Headers.Strings = (
      'Content-Type:application/json')
    ResponseType = rtText
    Left = 120
    Top = 240
  end
  object reqGetUsers: TWebHttpRequest
    Headers.Strings = (
      'Content-Type:application/json')
    ResponseType = rtText
    Left = 224
    Top = 664
  end
  object reqGetDoctorById: TWebHttpRequest
    Headers.Strings = (
      'Content-Type:application/json')
    ResponseType = rtText
    Left = 320
    Top = 664
  end
  object reqGetUserRole: TWebHttpRequest
    Headers.Strings = (
      'Content-Type:application/json')
    ResponseType = rtText
    Left = 112
    Top = 736
  end
  object reqPostUser: TWebHttpRequest
    Command = httpPOST
    Headers.Strings = (
      'Content-Type:application/json')
    ResponseType = rtText
    Left = 224
    Top = 736
  end
  object reqPostDoctor: TWebHttpRequest
    Command = httpPOST
    Headers.Strings = (
      'Content-Type:application/json')
    ResponseType = rtText
    Left = 328
    Top = 736
  end
  object reqPutDoctor: TWebHttpRequest
    Command = httpPUT
    Headers.Strings = (
      'Content-Type=application/json')
    ResponseType = rtText
    Left = 440
    Top = 736
  end
  object reqDeleteDoctor: TWebHttpRequest
    Command = httpDELETE
    Headers.Strings = (
      'Content-Type=application/json')
    ResponseType = rtText
    Left = 528
    Top = 736
  end
  object reqDeleteUser: TWebHttpRequest
    Command = httpDELETE
    Headers.Strings = (
      'Content-Type=application/json')
    ResponseType = rtText
    Left = 432
    Top = 664
  end
  object reqPostRegister: TWebHttpRequest
    Command = httpPOST
    Headers.Strings = (
      'Content-Type:application/json')
    ResponseType = rtText
    Left = 680
    Top = 464
  end
end
