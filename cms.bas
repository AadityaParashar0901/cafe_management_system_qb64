Screen _NewImage(960, 540, 32)
_Title "Cafe Management System"
Dim Shared As _Byte Page
Dim Shared As String Users, oldUsers, Passwords, oldPasswords: Users = ListNew$: Passwords = ListNew$: ListAdd Users, "Users": ListAdd Passwords, "Passwords"
Dim Shared As String Items, oldItems: Items = ListNew$: ListAdd Items, "Items"
Dim Shared As String ItemsPrice, oldItemsPrice: ItemsPrice = ListNew$: ListAdd ItemsPrice, "Price"
Dim Shared As String ItemsNumber, oldItemsNumber: ItemsNumber = ListNew$: ListAdd ItemsNumber, "ID"
Dim Shared As String Orders, oldOrders: Orders = ListNew$: ListAdd Orders, "Orders"
Dim Shared As String OrdersQuantity, oldOrdersQuantity: OrdersQuantity = ListNew$: ListAdd OrdersQuantity, "Quantity"
Dim Shared As String OrdersNumber, oldOrdersNumber: OrdersNumber = ListNew$: ListAdd OrdersNumber, "ID"
Dim Shared As String OrdersAmount, oldOrdersAmount: OrdersAmount = ListNew$: ListAdd OrdersAmount, "Amount"
Dim Shared As Long MouseWheel
If _FileExists("CONFIG") Then
    Open "CONFIG" For Binary As #1
    LOAD Users
    LOAD Passwords
    LOAD Items
    LOAD ItemsPrice
    LOAD Orders
    LOAD OrdersQuantity
    For I = 2 To ListLength(Items)
        ListAdd ItemsNumber, _Trim$(Str$(ListLength(ItemsNumber)))
    Next I
    For I = 2 To ListLength(Orders)
        ListAdd OrdersNumber, _Trim$(Str$(ListLength(OrdersNumber)))
        ListAdd OrdersAmount, _Trim$(Str$(Val(ListGet$(ItemsPrice, ListSearch(Items, ListGet$(Orders, I)))) * Val(ListGet$(OrdersQuantity, I))))
    Next I
    Close #1
Else
    ListAdd Users, "Admin"
    ListAdd Passwords, "Admin"
End If
Do
    _Limit 60
    While _MouseInput: MouseWheel = MouseWheel + _MouseWheel: Wend
    If Page = 0 Then
        Cls , -1
        Color _RGB32(0), 0
        LoginPage 0 'Login Page
    Else
        Cls , _RGB32(191)
        Color _RGB32(0), 0
        Line (0, 0)-(_Width * 0.3 - 1, _Height - 1), _RGB32(0, 63, 127), BF
        If Button(3, _Width * 0.15, _Height * 0.1, "Items") Then Page = 1
        If Button(3, _Width * 0.15, _Height * 0.2, "Users") Then Page = 2
        If Button(3, _Width * 0.15, _Height * 0.3, "Orders") Then Page = 3
        If Button(3, _Width * 0.15, _Height * 0.9, "Logout") Then
            Page = 0
            LoginPage 1
            ItemsPage 1
            UsersPage 1
        End If
        Select Case Page
            Case 1: _PrintString (_Width * 0.4, _Height * 0.1), "Items"
                ItemsPage 0
            Case 2: _PrintString (_Width * 0.4, _Height * 0.1), "Users"
                UsersPage 0
            Case 3: _PrintString (_Width * 0.4, _Height * 0.1), "Orders"
                OrdersPage 0
        End Select
    End If
    _Display
    If oldUsers <> Users Or oldPasswords <> Passwords Or oldItems <> Items Or oldItemsPrice <> ItemsPrice Or oldOrders <> Orders Or oldOrdersQuantity <> oldOrdersQuantity Then
        Open "CONFIG" For Output As #1
        Close #1
        Open "CONFIG" For Binary As #1
        SAVE Users
        oldUsers = Users
        SAVE Passwords
        oldPasswords = Passwords
        SAVE Items
        oldItems = Items
        SAVE ItemsPrice
        oldItemsPrice = ItemsPrice
        SAVE Orders
        oldOrders = Orders
        SAVE OrdersQuantity
        oldOrdersQuantity = OrdersQuantity
        Close #1
    End If
Loop Until Inp(&H60) = 1
System
Sub LOAD (T$)
    Get #1, , L~&: T$ = String$(L~&, 0): Get #1, , T$
End Sub
Sub SAVE (T$)
    L~& = Len(T$): Put #1, , L~&: Put #1, , T$
End Sub
Sub LoginPage (MODE~%%)
    Static focusTextBox
    Static UserName$, Password$
    If MODE~%% = 1 Then
        focusTextBox = 0
        UserName$ = ""
        Password$ = ""
        Exit Sub
    End If
    KeyHit = _KeyHit
    If KeyHit = 13 Then
        If focusTextBox = 1 Then Page = 1
        If focusTextBox = 0 Then focusTextBox = 1
        While _KeyDown(13): Wend
    End If
    If _MouseButton(1) Then
        If InRange(_Width / 2 - 200, _MouseX, _Width / 2 + 199) Then
            If InRange(_Height * 0.5 - 10, _MouseY, _Height * 0.5 + 9) Then focusTextBox = 0
            If InRange(_Height * 0.6 - 10, _MouseY, _Height * 0.6 + 9) Then focusTextBox = 1
        End If
    End If
    InputBox _Width / 2, _Height * 0.5, UserName$, "User Name", focusTextBox = 0, KeyHit, 0
    InputBox _Width / 2, _Height * 0.6, Password$, "Password", focusTextBox = 1, KeyHit, 1
    If Button(1, _Width / 2, _Height * 0.8, "Login") Then
        UserName_SearchIndex = ListSearch(Users, UserName$)
        Password_SearchIndex = ListSearch(Passwords, Password$)
        If UserName_SearchIndex * Password_SearchIndex <> 0 And UserName_SearchIndex = Password_SearchIndex Then
            Page = 1
        Else
            System
        End If
    End If
End Sub
Sub ItemsPage (MODE~%%)
    Static ItemsScroll&, ItemSelected~&: If ItemsScroll& = 0 Then ItemsScroll& = 1
    Static ItemName$, ItemPrice$
    Static focusTextBox
    If MODE~%% = 1 Then
        focusTextBox = 0
        ItemName$ = ""
        ItemPrice$ = ""
        ItemsScroll& = 1
        ItemSelected~& = 0
        Exit Sub
    End If
    ListBox _Width * 0.4, _Height * 0.2, ItemsNumber, ItemsScroll&, ItemSelected~&, 1
    ListBox _Width * 0.5, _Height * 0.2, Items, ItemsScroll&, ItemSelected~&, 1
    ListBox _Width * 0.6, _Height * 0.2, ItemsPrice, ItemsScroll&, ItemSelected~&, 1
    If ItemSelected~& = 1 Then ItemSelected~& = 0
    KeyHit = _KeyHit
    If KeyHit = 13 Then
        If focusTextBox = 1 Then focusTextBox = 2
        While _KeyDown(13): Wend
    End If
    If _MouseButton(1) Then
        If InRange(_Width * 0.55 - 200, _MouseX, _Width * 0.55 + 199) And InRange(_Height * 0.75 - 10, _MouseY, _Height * 0.75 + 9) Then focusTextBox = 1
        If InRange(_Width * 0.55 - 200, _MouseX, _Width * 0.55 + 199) And InRange(_Height * 0.8 - 10, _MouseY, _Height * 0.8 + 9) Then focusTextBox = 2
    End If
    InputBox _Width * 0.55, _Height * 0.75, ItemName$, "Item Name", focusTextBox = 1, KeyHit, 0
    InputBox _Width * 0.55, _Height * 0.8, ItemPrice$, "Item Price", focusTextBox = 2, KeyHit, 0
    If Button(3, _Width * 0.4, _Height * 0.9, "Add") And Len(ItemName$) > 0 And Len(ItemPrice$) > 0 Then
        ListAdd Items, ItemName$
        ListAdd ItemsPrice, ItemPrice$
        ListAdd ItemsNumber, _Trim$(Str$(ListLength(ItemsNumber)))
        ItemName$ = ""
        ItemPrice$ = ""
    End If
    If Button(3, _Width * 0.55, _Height * 0.9, "Delete") And ItemSelected~& Then
        ListDelete Items, ItemSelected~&
        ListDelete ItemsPrice, ItemSelected~&
        ListDelete ItemsNumber, ListLength(ItemsNumber)
        ItemSelected~& = 0
    End If
End Sub
Sub UsersPage (MODE~%%)
    Static focusTextBox
    Static UserName$, Password$
    Static UsersScroll&, UserSelected~&: If UsersScroll& = 0 Then UsersScroll& = 1
    If MODE~%% = 1 Then
        focusTextBox = 0
        UserName$ = ""
        Password$ = ""
        UsersScroll& = 1
        UserSelected~& = 0
        Exit Sub
    End If
    ListBox _Width * 0.4, _Height * 0.2, Users, UsersScroll&, UserSelected~&, 1
    ListBox _Width * 0.5, _Height * 0.2, Passwords, UsersScroll&, UserSelected~&, 1
    If UserSelected~& = 1 Then UserSelected~& = 0
    KeyHit = _KeyHit
    If KeyHit = 13 Then
        If focusTextBox = 1 Then focusTextBox = 2
        While _KeyDown(13): Wend
    End If
    If _MouseButton(1) Then
        If InRange(_Width * 0.55 - 200, _MouseX, _Width * 0.55 + 199) Then
            If InRange(_Height * 0.75 - 10, _MouseY, _Height * 0.75 + 9) Then focusTextBox = 1
            If InRange(_Height * 0.8 - 10, _MouseY, _Height * 0.8 + 9) Then focusTextBox = 2
        End If
    End If
    InputBox _Width * 0.55, _Height * 0.75, UserName$, "User Name", focusTextBox = 1, KeyHit, 0
    InputBox _Width * 0.55, _Height * 0.8, Password$, "Password", focusTextBox = 2, KeyHit, 0
    If Button(3, _Width * 0.4, _Height * 0.9, "Add") And Len(UserName$) > 0 And Len(Password$) > 0 Then
        ListAdd Users, UserName$
        ListAdd Passwords, Password$
        UserName$ = ""
        Password$ = ""
    End If
    If Button(3, _Width * 0.55, _Height * 0.9, "Delete") And UserSelected~& Then
        ListDelete Users, UserSelected~&
        ListDelete Passwords, UserSelected~&
        UserSelected~& = 0
    End If
End Sub
Sub OrdersPage (MODE~%%)
    Static focusTextBox
    Static OrderName$, OrderQuantity$
    Static OrderScroll&, OrderSelected~&: If OrderScroll& = 0 Then OrderScroll& = 1
    If MODE~%% = 1 Then
        focusTextBox = 0
        OrderName$ = ""
        OrderQuantity$ = ""
        OrderScroll& = 1
        OrderSelected~& = 0
        Exit Sub
    End If
    ListBox _Width * 0.4, _Height * 0.2, OrdersNumber, OrderScroll&, OrderSelected~&, 1
    ListBox _Width * 0.5, _Height * 0.2, Orders, OrderScroll&, OrderSelected~&, 1
    ListBox _Width * 0.6, _Height * 0.2, OrdersQuantity, OrderScroll&, OrderSelected~&, 1
    ListBox _Width * 0.7, _Height * 0.2, OrdersAmount, OrderScroll&, OrderSelected~&, 1
    If OrderSelected~& = 1 Then OrderSelected~& = 0
    KeyHit = _KeyHit
    If KeyHit = 13 Then
        If focustestbox = 1 Then focusTextBox = 2
        While _KeyDown(13): Wend
    End If
    If _MouseButton(1) Then
        If InRange(_Width * 0.55 - 200, _MouseX, _Width * 0.55 + 199) Then
            If InRange(_Height * 0.75 - 10, _MouseY, _Height * 0.75 + 9) Then focusTextBox = 1
            If InRange(_Height * 0.8 - 10, _MouseY, _Height * 0.8 + 9) Then focusTextBox = 2
        End If
    End If
    InputBox _Width * 0.55, _Height * 0.75, OrderName$, "Order", focusTextBox = 1, KeyHit, 0
    InputBox _Width * 0.55, _Height * 0.8, OrderQuantity$, "Quantity", focusTextBox = 2, KeyHit, 0
    If Button(3, _Width * 0.4, _Height * 0.9, "Add") And Len(OrderName$) > 0 And Len(OrderQuantity$) > 0 Then
        If ListSearch(Items, OrderName$) Then
            ListAdd Orders, OrderName$
            ListAdd OrdersQuantity, OrderQuantity$
            ListAdd OrdersNumber, _Trim$(Str$(ListLength(OrdersNumber)))
            ListAdd OrdersAmount, _Trim$(Str$(Val(ListGet$(ItemsPrice, ListSearch(Items, OrderName$))) * Val(OrderQuantity$)))
            OrderName$ = ""
            OrderQuantity$ = ""
        End If
    End If
    If Button(3, _Width * 0.55, _Height * 0.9, "Delete") And OrderSelected~& Then
        ListDelete Orders, OrderSelected~&
        ListDelete OrdersQuantity, OrderSelected~&
        ListDelete OrdersNumber, ListLength(OrdersNumber)
        ListDelete OrdersAmount, OrderSelected~&
        OrderSelected~& = 0
    End If
End Sub
Function Button (T~%%, X, Y, S$)
    Select Case T~%%
        Case 1:
            If InRange(X - 200, _MouseX, X + 199) And InRange(Y - 20, _MouseY, Y + 19) Then
                _PutImage (X - 200, Y - 20)-(X + 199, Y + 19), load_button_highlighted&
                If _MouseButton(1) Then Button = -1
                While _MouseButton(1) Or _MouseInput: Wend
            Else
                _PutImage (X - 200, Y - 20)-(X + 199, Y + 19), load_button&
            End If
            _PrintString (X - Len(S$) * _FontWidth / 2, Y - _FontHeight / 2), S$
        Case 2:
            _PutImage (X - 200, Y - 20)-(X + 199, Y + 19), load_button_disabled&
            _PrintString (X - Len(S$) * _FontWidth / 2, Y - _FontHeight / 2), S$
        Case 3:
            If InRange(X - 50, _MouseX, X + 49) And InRange(Y - 20, _MouseY, Y + 19) Then
                _PutImage (X - 50, Y - 20)-(X + 49, Y + 19), load_small_button_highlighted&
                If _MouseButton(1) Then Button = -1
                While _MouseButton(1) Or _MouseInput: Wend
            Else
                _PutImage (X - 50, Y - 20)-(X + 49, Y + 19), load_small_button&
            End If
            _PrintString (X - Len(S$) * _FontWidth / 2, Y - _FontHeight / 2), S$
    End Select
End Function
Sub InputBox (X, Y, S$, H$, isInFocus, KeyHit, isPassword~%%)
    _PutImage (X - 200, Y - 10)-(X + 199, Y + 9), load_inputbox&
    If isInFocus And 2 * Timer(0.1) - Int(2 * Timer) > 0.5 Then C$ = "_"
    T$ = Right$(S$, 400 \ _FontWidth)
    If isPassword~%% Then T$ = String$(Len(T$), 7)
    _PrintString (X - 200, Y - _FontHeight / 2), T$ + C$
    If Len(T$) = 0 Then _PrintString (X - Len(H$) * _FontWidth / 2, Y - _FontHeight / 2), H$
    If isInFocus = 0 Then Exit Sub
    Select Case KeyHit
        Case 8: S$ = Left$(S$, Len(S$) - 1)
        Case 32 To 126: S$ = S$ + Chr$(KeyHit)
    End Select
End Sub
Sub ListBox (X, Y, L$, Scroll&, Selected~&, firstItemColor~%%)
    Dim As Long I
    Const FontHeight = 18
    LL~& = ListLength(L$)
    Line (X - 50, Y)-(X + 49, Y + FontHeight * Min(LL~&, 16)), -1, BF
    For I = Scroll& To Min(LL~&, Scroll& + 16)
        Color _RGB32(0), 0
        If I = 1 And firstItemColor~%% Then
            Line (X - 50, Y)-(X + 49, Y + FontHeight), _RGB32(223), BF
        Else
            If InRange(X - 50, _MouseX, X + 49) Then
                If InRange(Y + (I - 1) * FontHeight, _MouseY, Y + I * FontHeight) Then
                    Color -1, 0
                    Line (X - 50, Y + (I - 1) * FontHeight)-(X + 49, Y + I * FontHeight), _RGB32(0, 127, 255, 127), BF
                    If _MouseButton(1) Then Selected~& = I
                End If
            End If
            If Selected~& = I Then Line (X - 50, Y + (I - 1) * FontHeight)-(X + 49, Y + I * FontHeight), _RGB32(0, 63, 255), BF
            If Selected~& = I Then Color -1, 0
        End If
        Line (X - 50, Y + (I - 1) * FontHeight)-(X + 49, Y + I * FontHeight), _RGB32(0), B
        _PrintString (X - 49, Y + (I - 1) * FontHeight + 1), Left$(ListGet$(L$, I), 98 / _FontWidth)
    Next I
    Color _RGB32(0), 0
    If InRange(X - 50, _MouseX, X + 49) And InRange(Y, _MouseY, Y + FontHeight * Min(LL~&, 16)) Then Scroll& = Min!(LL~&, Max(1, Scroll& + MouseWheel))
End Sub
Function Min! (A!, B!)
    Min! = -A! * (A! < B!) - B! * (A! >= B!)
End Function
Function Max! (A!, B!)
    Max! = -A! * (A! > B!) - B! * (A! <= B!)
End Function
Function InRange~%% (A!, B!, C!)
    InRange~%% = (A! <= B!) And (B! <= C!)
End Function
'$Include:'List.bas'
'----------------------------------------------------------------
'$Include:'assets\button.png.bi'
'$Include:'assets\button_disabled.png.bi'
'$Include:'assets\button_highlighted.png.bi'
'$Include:'assets\small_button.png.bi'
'$Include:'assets\small_button_highlighted.png.bi'
'$Include:'assets\inputbox.png.bi'
'----------------------------------------------------------------
