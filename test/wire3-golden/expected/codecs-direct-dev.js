function $author$project$Other$w3d_Opaque() {
  switch (_W3_ru8()) {
    case 0: return $author$project$Other$Opaque({ a: _W3_rstr(), inner: _W3_rint() });
    default: throw 0;
  }
function $author$project$Other$w3d_OtherCtor() {
  switch (_W3_ru8()) {
    case 0: return $author$project$Other$Al(_W3_rint());
    case 1: return $author$project$Other$B;
    case 2: return $author$project$Other$Bb($author$project$Other$w3dh_OtherCtor_0());
    case 3: return $author$project$Other$MiddleOne$(_W3_rstr(), _W3_rbool());
    case 4: return $author$project$Other$Zulu;
    default: throw 0;
  }
function $author$project$Other$w3d_OtherRec() {
  return { a: $author$project$Other$w3dh_OtherRec_0(), ab: $author$project$Other$w3dh_OtherRec_1(), abc: _Utils_Tuple2(_W3_rint(), _W3_rf64()), zebra: $author$project$Other$w3d_OtherCtor() };
function $author$project$Other$w3d_OtherRec2() {
  return { x: _W3_rstr(), yy: _W3_rint() };
function $author$project$Other$w3d_Person() {
  return { a: _W3_rf64(), age: _W3_rint(), b: _W3_rbool(), longest: _W3_rstr(), name: _W3_rstr(), zid: _W3_rint() };
function $author$project$Other$w3dh_OtherCtor_0() {
  var r = _List_Nil, t = null;
  for (var n = _W3_rint(); n > 0; n--) {
    var c = _List_Cons($author$project$Other$w3d_OtherCtor(), _List_Nil);
    if (t === null) r = c; else t.b = c;
    t = c;
  }
  return r;
function $author$project$Other$w3dh_OtherRec_0() {
  switch (_W3_ru8()) {
    case 0: return $elm$core$Maybe$Nothing;
    case 1: return $elm$core$Maybe$Just($author$project$Other$w3d_OtherRec2());
    default: throw 0;
  }
function $author$project$Other$w3dh_OtherRec_1() {
  var ks = [null], vs = [null];
  ks.length = 0; vs.length = 0;
  for (var n = _W3_rint(); n > 0; n--) {
    ks.push(_W3_rstr());
    vs.push(_W3_rint());
  }
  return _W3_dictFromPairs(ks, vs);
function $author$project$Other$w3e_Opaque(v) {
  _W3_u8(0);
  _W3_str(v.a.a);
  _W3_int(v.a.inner);
function $author$project$Other$w3e_OtherCtor(v) {
  switch (v.$) {
    case 'Al':
      _W3_u8(0);
      _W3_int(v.a);
      return;
    case 'B':
      _W3_u8(1);
      return;
    case 'Bb':
      _W3_u8(2);
      var n1 = 0;
      for (var c1 = v.a; c1.b; c1 = c1.b) n1++;
      _W3_int(n1);
      for (var c1 = v.a; c1.b; c1 = c1.b) {
        $author$project$Other$w3e_OtherCtor(c1.a);
      }
      return;
    case 'MiddleOne':
      _W3_u8(3);
      _W3_str(v.a);
      _W3_u8(v.b ? 1 : 0);
      return;
    case 'Zulu':
      _W3_u8(4);
      return;
  }
function $author$project$Other$w3e_OtherRec(v) {
  var m1 = v.a;
  if (m1.$ === 'Nothing') { _W3_u8(0); } else {
    _W3_u8(1);
    $author$project$Other$w3e_OtherRec2(m1.a);
  }
  _W3_int(_W3_dictSize(v.ab));
  var st1 = [], nd1 = v.ab;
  for (;;) {
    while (nd1.$ === _W3_T_RBNODE) { st1.push(nd1); nd1 = nd1.d; }
    if (st1.length === 0) break;
    nd1 = st1.pop();
    _W3_str(nd1.b);
    _W3_int(nd1.c);
    nd1 = nd1.e;
  }
  _W3_int(v.abc.a);
  _W3_f64(v.abc.b);
  $author$project$Other$w3e_OtherCtor(v.zebra);
function $author$project$Other$w3e_OtherRec2(v) {
  _W3_str(v.x);
  _W3_int(v.yy);
function $author$project$Other$w3e_Person(v) {
  _W3_f64(v.a);
  _W3_int(v.age);
  _W3_u8(v.b ? 1 : 0);
  _W3_str(v.longest);
  _W3_str(v.name);
  _W3_int(v.zid);
function $author$project$Types$w3d_BackendModel() {
  return { choice: $author$project$Types$w3d_Choice($author$project$Types$w3da_BackendModel_0, $author$project$Types$w3da_BackendModel_1), containers: $author$project$Types$w3d_Containers(), cross: $author$project$Types$w3d_Cross(), forest: $author$project$Types$w3d_Forest(), trees: $author$project$Types$w3dh_BackendModel_2(), usesExt: $author$project$Types$w3d_UsesExt() };
function $author$project$Types$w3d_BackendMsg() {
  switch (_W3_ru8()) {
    case 0: return $author$project$Types$BNoop;
    case 1: return $author$project$Types$Bb_($author$project$Types$w3d_Cross());
    case 2: return $author$project$Types$Bz(_W3_rint());
    default: throw 0;
  }
function $author$project$Types$w3d_Choice(d0, d1) {
  switch (_W3_ru8()) {
    case 0: return $author$project$Types$Al(d1());
    case 1: return $author$project$Types$Middle$(d0(), d1());
    case 2: return $author$project$Types$None_;
    case 3: return $author$project$Types$Zed(d0());
    default: throw 0;
  }
function $author$project$Types$w3d_Containers() {
  return { array: $elm$core$Array$fromList($author$project$Types$w3dh_Containers_0()), bytes: _W3_rbytes(), dict: $author$project$Types$w3dh_Containers_1(), dictTuple: $author$project$Types$w3dh_Containers_2(), list: $author$project$Types$w3dh_Containers_3(), maybe: $author$project$Types$w3dh_Containers_4(), order: _W3_rorder(), result: $author$project$Types$w3dh_Containers_5(), resultOk: $author$project$Types$w3dh_Containers_7(), seqDict: $lamdera$containers$SeqDict$fromList($author$project$Types$w3dh_Containers_8()), seqDictCtor: $lamdera$containers$SeqDict$fromList($author$project$Types$w3dh_Containers_10()), seqSet: $lamdera$containers$SeqSet$fromList($author$project$Types$w3dh_Containers_11()), seqSetRec: $lamdera$containers$SeqSet$fromList($author$project$Types$w3dh_Containers_12()), set: $author$project$Types$w3dh_Containers_13(), time: $elm$time$Time$millisToPosix(_W3_rint()), triple: _Utils_Tuple3(_W3_rbool(), _W3_rf64(), $author$project$Types$w3d_Ctors()), unit: _Utils_Tuple0, zTuple: _Utils_Tuple2(_W3_rint(), _W3_rstr()) };
function $author$project$Types$w3d_Cross() {
  return { ctor: $author$project$Other$w3d_OtherCtor(), opaque: $author$project$Other$w3d_Opaque(), other: $author$project$Other$w3d_OtherRec(), tree: $author$project$Types$w3d_Tree($author$project$Types$w3da_Cross_2), wrapped: $author$project$Types$w3d_Wrapped($author$project$Types$w3da_Cross_3) };
function $author$project$Types$w3d_Ctors() {
  switch (_W3_ru8()) {
    case 0: return $author$project$Types$A;
    case 1: return $author$project$Types$AB;
    case 2: return $author$project$Types$A_b;
    case 3: return $author$project$Types$Aaa;
    case 4: return $author$project$Types$Ab$(_W3_rstr(), _W3_rf64());
    case 5: return $author$project$Types$B2(_W3_rint());
    case 6: return $author$project$Types$Ba($author$project$Types$w3dh_Ctors_0());
    case 7: return $author$project$Types$LongerName;
    case 8: return $author$project$Types$Mid;
    case 9: return $author$project$Types$Zz;
    default: throw 0;
  }
function $author$project$Types$w3d_Forest() {
  switch (_W3_ru8()) {
    case 0: return $author$project$Types$Forest({ kids: $author$project$Types$w3dh_Forest_0(), nm: _W3_rstr(), z: _W3_rint() });
    default: throw 0;
  }
function $author$project$Types$w3d_FrontendModel() {
  return { a: $author$project$Types$w3d_Ctors(), key: _W3_rint(), nested: $author$project$Types$w3d_Nested(), zeta: $author$project$Types$w3d_Rec() };
function $author$project$Types$w3d_FrontendMsg() {
  switch (_W3_ru8()) {
    case 0: return $author$project$Types$F($author$project$Types$w3d_Choice($author$project$Types$w3d_Ctors, $author$project$Types$w3d_Nested));
    case 1: return $author$project$Types$FNoop;
    case 2: return $author$project$Types$FaMsg;
    case 3: return $author$project$Types$FzMsg($author$project$Types$w3d_Rec());
    default: throw 0;
  }
function $author$project$Types$w3d_Nested() {
  return { list: $author$project$Types$w3dh_Nested_0(), outer: { aInner: { x: _W3_rint(), xLonger: _W3_rf64(), yy: _W3_rstr() }, zInner: _W3_rint() }, rec: $author$project$Types$w3d_Rec() };
function $author$project$Types$w3d_Rec() {
  return { a: _W3_rstr(), aaa: _Utils_Tuple0, ab: $author$project$Types$w3dh_Rec_0(), b1: _W3_rstr(), b2: _Char_fromCode(_W3_rint()), bA: _W3_rorder(), b_: _W3_rint(), longerName: _W3_rbool(), mid: _W3_rf64(), zeta: _W3_rint(), zz: $author$project$Types$w3dh_Rec_1() };
function $author$project$Types$w3d_ToBackend() {
  switch (_W3_ru8()) {
    case 0: return $author$project$Types$Tb;
    case 1: return $author$project$Types$Tb2($author$project$Types$w3d_UsesExt());
    case 2: return $author$project$Types$ToBackendA($author$project$Types$w3d_Choice($author$project$Types$w3da_ToBackend_0, $author$project$Types$w3da_ToBackend_1));
    case 3: return $author$project$Types$ToBackendZ($author$project$Types$w3d_Ctors());
    default: throw 0;
  }
function $author$project$Types$w3d_ToFrontend() {
  switch (_W3_ru8()) {
    case 0: return $author$project$Types$T($author$project$Types$w3d_Containers());
    case 1: return $author$project$Types$Tf;
    case 2: return $author$project$Types$TfLonger($author$project$Types$w3d_Nested());
    default: throw 0;
  }
function $author$project$Types$w3d_Tree(d0) {
  switch (_W3_ru8()) {
    case 0: return $author$project$Types$Leaf;
    case 1: return $author$project$Types$Node$($author$project$Types$w3d_Tree(d0), d0(), $author$project$Types$w3d_Tree(d0));
    default: throw 0;
  }
function $author$project$Types$w3d_UsesExt() {
  return { e: { k: _W3_rint(), kk: _W3_rbool(), m: $author$project$Types$w3dh_UsesExt_0(), zz: $author$project$Types$w3dh_UsesExt_1() }, p: { a: _W3_rf64(), age: _W3_rint(), b: _W3_rbool(), longest: _W3_rstr(), name: _W3_rstr(), zid: _W3_rint() }, q: { a: _W3_rf64(), ex: _W3_rint(), extra: _W3_rstr(), name: _W3_rstr(), zid: _W3_rint() } };
function $author$project$Types$w3d_Wrapped(d0) {
  return { at: $author$project$Types$w3dh_Wrapped_0(d0), value: d0(), zcount: _W3_rint() };
function $author$project$Types$w3dh_BackendModel_2() {
  var r = _List_Nil, t = null;
  for (var n = _W3_rint(); n > 0; n--) {
    var c = _List_Cons($author$project$Types$w3d_Tree($author$project$Types$w3d_Ctors), _List_Nil);
    if (t === null) r = c; else t.b = c;
    t = c;
  }
  return r;
function $author$project$Types$w3dh_Containers_0() {
  var r = _List_Nil, t = null;
  for (var n = _W3_rint(); n > 0; n--) {
    var c = _List_Cons($author$project$Types$w3d_Ctors(), _List_Nil);
    if (t === null) r = c; else t.b = c;
    t = c;
  }
  return r;
function $author$project$Types$w3dh_Containers_1() {
  var ks = [null], vs = [null];
  ks.length = 0; vs.length = 0;
  for (var n = _W3_rint(); n > 0; n--) {
    ks.push(_W3_rstr());
    vs.push($author$project$Types$w3d_Rec());
  }
  return _W3_dictFromPairs(ks, vs);
function $author$project$Types$w3dh_Containers_10() {
  var r = _List_Nil, t = null;
  for (var n = _W3_rint(); n > 0; n--) {
    var c = _List_Cons(_Utils_Tuple2($author$project$Types$w3d_Ctors(), $author$project$Types$w3dh_Containers_9()), _List_Nil);
    if (t === null) r = c; else t.b = c;
    t = c;
  }
  return r;
function $author$project$Types$w3dh_Containers_11() {
  var r = _List_Nil, t = null;
  for (var n = _W3_rint(); n > 0; n--) {
    var c = _List_Cons($author$project$Types$w3d_Ctors(), _List_Nil);
    if (t === null) r = c; else t.b = c;
    t = c;
  }
  return r;
function $author$project$Types$w3dh_Containers_12() {
  var r = _List_Nil, t = null;
  for (var n = _W3_rint(); n > 0; n--) {
    var c = _List_Cons($author$project$Types$w3d_Rec(), _List_Nil);
    if (t === null) r = c; else t.b = c;
    t = c;
  }
  return r;
function $author$project$Types$w3dh_Containers_13() {
  var ks = [null], vs = [null];
  ks.length = 0; vs.length = 0;
  for (var n = _W3_rint(); n > 0; n--) {
    ks.push(_W3_rstr());
    vs.push(_Utils_Tuple0);
  }
  return $elm$core$Set$Set_elm_builtin(_W3_dictFromPairs(ks, vs));
function $author$project$Types$w3dh_Containers_2() {
  var ks = [null], vs = [null];
  ks.length = 0; vs.length = 0;
  for (var n = _W3_rint(); n > 0; n--) {
    ks.push(_Utils_Tuple2(_W3_rint(), _W3_rstr()));
    vs.push(_W3_rf64());
  }
  return _W3_dictFromPairs(ks, vs);
function $author$project$Types$w3dh_Containers_3() {
  var r = _List_Nil, t = null;
  for (var n = _W3_rint(); n > 0; n--) {
    var c = _List_Cons($author$project$Types$w3d_Rec(), _List_Nil);
    if (t === null) r = c; else t.b = c;
    t = c;
  }
  return r;
function $author$project$Types$w3dh_Containers_4() {
  switch (_W3_ru8()) {
    case 0: return $elm$core$Maybe$Nothing;
    case 1: return $elm$core$Maybe$Just($author$project$Types$w3d_Rec());
    default: throw 0;
  }
function $author$project$Types$w3dh_Containers_5() {
  switch (_W3_ru8()) {
    case 0: return $elm$core$Result$Ok($author$project$Types$w3d_Ctors());
    case 1: return $elm$core$Result$Err(_W3_rstr());
    default: throw 0;
  }
function $author$project$Types$w3dh_Containers_6() {
  var r = _List_Nil, t = null;
  for (var n = _W3_rint(); n > 0; n--) {
    var c = _List_Cons($author$project$Types$w3d_Ctors(), _List_Nil);
    if (t === null) r = c; else t.b = c;
    t = c;
  }
  return r;
function $author$project$Types$w3dh_Containers_7() {
  switch (_W3_ru8()) {
    case 0: return $elm$core$Result$Ok($author$project$Types$w3dh_Containers_6());
    case 1: return $elm$core$Result$Err(_W3_rint());
    default: throw 0;
  }
function $author$project$Types$w3dh_Containers_8() {
  var r = _List_Nil, t = null;
  for (var n = _W3_rint(); n > 0; n--) {
    var c = _List_Cons(_Utils_Tuple2($author$project$Types$w3d_Rec(), _W3_rint()), _List_Nil);
    if (t === null) r = c; else t.b = c;
    t = c;
  }
  return r;
function $author$project$Types$w3dh_Containers_9() {
  var r = _List_Nil, t = null;
  for (var n = _W3_rint(); n > 0; n--) {
    var c = _List_Cons($author$project$Types$w3d_Ctors(), _List_Nil);
    if (t === null) r = c; else t.b = c;
    t = c;
  }
  return r;
function $author$project$Types$w3dh_Ctors_0() {
  switch (_W3_ru8()) {
    case 0: return $elm$core$Maybe$Nothing;
    case 1: return $elm$core$Maybe$Just($author$project$Types$w3d_Ctors());
    default: throw 0;
  }
function $author$project$Types$w3dh_Forest_0() {
  var r = _List_Nil, t = null;
  for (var n = _W3_rint(); n > 0; n--) {
    var c = _List_Cons($author$project$Types$w3d_Forest(), _List_Nil);
    if (t === null) r = c; else t.b = c;
    t = c;
  }
  return r;
function $author$project$Types$w3dh_Nested_0() {
  var r = _List_Nil, t = null;
  for (var n = _W3_rint(); n > 0; n--) {
    var c = _List_Cons({ pp: _W3_rstr(), q: _W3_rint(), r: _W3_rbool() }, _List_Nil);
    if (t === null) r = c; else t.b = c;
    t = c;
  }
  return r;
function $author$project$Types$w3dh_Rec_0() {
  var r = _List_Nil, t = null;
  for (var n = _W3_rint(); n > 0; n--) {
    var c = _List_Cons(_W3_rint(), _List_Nil);
    if (t === null) r = c; else t.b = c;
    t = c;
  }
  return r;
function $author$project$Types$w3dh_Rec_1() {
  switch (_W3_ru8()) {
    case 0: return $elm$core$Maybe$Nothing;
    case 1: return $elm$core$Maybe$Just(_W3_rint());
    default: throw 0;
  }
function $author$project$Types$w3dh_UsesExt_0() {
  switch (_W3_ru8()) {
    case 0: return $elm$core$Maybe$Nothing;
    case 1: return $elm$core$Maybe$Just(_W3_rstr());
    default: throw 0;
  }
function $author$project$Types$w3dh_UsesExt_1() {
  var r = _List_Nil, t = null;
  for (var n = _W3_rint(); n > 0; n--) {
    var c = _List_Cons(_W3_rint(), _List_Nil);
    if (t === null) r = c; else t.b = c;
    t = c;
  }
  return r;
function $author$project$Types$w3dh_Wrapped_0(d0) {
  var r = _List_Nil, t = null;
  for (var n = _W3_rint(); n > 0; n--) {
    var c = _List_Cons(d0(), _List_Nil);
    if (t === null) r = c; else t.b = c;
    t = c;
  }
  return r;
function $author$project$Types$w3e_BackendModel(v) {
  $author$project$Types$w3e_Choice(v.choice, $author$project$Types$w3ea_BackendModel_0, $author$project$Types$w3ea_BackendModel_1);
  $author$project$Types$w3e_Containers(v.containers);
  $author$project$Types$w3e_Cross(v.cross);
  $author$project$Types$w3e_Forest(v.forest);
  var n1 = 0;
  for (var c1 = v.trees; c1.b; c1 = c1.b) n1++;
  _W3_int(n1);
  for (var c1 = v.trees; c1.b; c1 = c1.b) {
    $author$project$Types$w3e_Tree(c1.a, $author$project$Types$w3e_Ctors);
  }
  $author$project$Types$w3e_UsesExt(v.usesExt);
function $author$project$Types$w3e_BackendMsg(v) {
  switch (v.$) {
    case 'BNoop':
      _W3_u8(0);
      return;
    case 'Bb_':
      _W3_u8(1);
      $author$project$Types$w3e_Cross(v.a);
      return;
    case 'Bz':
      _W3_u8(2);
      _W3_int(v.a);
      return;
  }
function $author$project$Types$w3e_Choice(v, e0, e1) {
  switch (v.$) {
    case 'Al':
      _W3_u8(0);
      e1(v.a);
      return;
    case 'Middle':
      _W3_u8(1);
      e0(v.a);
      e1(v.b);
      return;
    case 'None_':
      _W3_u8(2);
      return;
    case 'Zed':
      _W3_u8(3);
      e0(v.a);
      return;
  }
function $author$project$Types$w3e_Containers(v) {
  var a1 = _W3_arrayToJs(v.array);
  _W3_int(a1.length);
  for (var i1 = 0; i1 < a1.length; i1++) {
    $author$project$Types$w3e_Ctors(a1[i1]);
  }
  _W3_bytes(v.bytes);
  _W3_int(_W3_dictSize(v.dict));
  var st1 = [], nd1 = v.dict;
  for (;;) {
    while (nd1.$ === _W3_T_RBNODE) { st1.push(nd1); nd1 = nd1.d; }
    if (st1.length === 0) break;
    nd1 = st1.pop();
    _W3_str(nd1.b);
    $author$project$Types$w3e_Rec(nd1.c);
    nd1 = nd1.e;
  }
  _W3_int(_W3_dictSize(v.dictTuple));
  var st1 = [], nd1 = v.dictTuple;
  for (;;) {
    while (nd1.$ === _W3_T_RBNODE) { st1.push(nd1); nd1 = nd1.d; }
    if (st1.length === 0) break;
    nd1 = st1.pop();
    _W3_int(nd1.b.a);
    _W3_str(nd1.b.b);
    _W3_f64(nd1.c);
    nd1 = nd1.e;
  }
  var n1 = 0;
  for (var c1 = v.list; c1.b; c1 = c1.b) n1++;
  _W3_int(n1);
  for (var c1 = v.list; c1.b; c1 = c1.b) {
    $author$project$Types$w3e_Rec(c1.a);
  }
  var m1 = v.maybe;
  if (m1.$ === 'Nothing') { _W3_u8(0); } else {
    _W3_u8(1);
    $author$project$Types$w3e_Rec(m1.a);
  }
  _W3_u8(v.order.$ === 'LT' ? 0 : v.order.$ === 'EQ' ? 1 : 2);
  var rs1 = v.result;
  if (rs1.$ === 'Ok') {
    _W3_u8(0);
    $author$project$Types$w3e_Ctors(rs1.a);
  } else {
    _W3_u8(1);
    _W3_str(rs1.a);
  }
  var rs1 = v.resultOk;
  if (rs1.$ === 'Ok') {
    _W3_u8(0);
    var n2 = 0;
    for (var c2 = rs1.a; c2.b; c2 = c2.b) n2++;
    _W3_int(n2);
    for (var c2 = rs1.a; c2.b; c2 = c2.b) {
      $author$project$Types$w3e_Ctors(c2.a);
    }
  } else {
    _W3_u8(1);
    _W3_int(rs1.a);
  }
  var l1 = $lamdera$containers$SeqDict$toList(v.seqDict);
  var n2 = 0;
  for (var c2 = l1; c2.b; c2 = c2.b) n2++;
  _W3_int(n2);
  for (var c2 = l1; c2.b; c2 = c2.b) {
    $author$project$Types$w3e_Rec(c2.a.a);
    _W3_int(c2.a.b);
  }
  var l1 = $lamdera$containers$SeqDict$toList(v.seqDictCtor);
  var n2 = 0;
  for (var c2 = l1; c2.b; c2 = c2.b) n2++;
  _W3_int(n2);
  for (var c2 = l1; c2.b; c2 = c2.b) {
    $author$project$Types$w3e_Ctors(c2.a.a);
    var n4 = 0;
    for (var c4 = c2.a.b; c4.b; c4 = c4.b) n4++;
    _W3_int(n4);
    for (var c4 = c2.a.b; c4.b; c4 = c4.b) {
      $author$project$Types$w3e_Ctors(c4.a);
    }
  }
  var l1 = $lamdera$containers$SeqSet$toList(v.seqSet);
  var n2 = 0;
  for (var c2 = l1; c2.b; c2 = c2.b) n2++;
  _W3_int(n2);
  for (var c2 = l1; c2.b; c2 = c2.b) {
    $author$project$Types$w3e_Ctors(c2.a);
  }
  var l1 = $lamdera$containers$SeqSet$toList(v.seqSetRec);
  var n2 = 0;
  for (var c2 = l1; c2.b; c2 = c2.b) n2++;
  _W3_int(n2);
  for (var c2 = l1; c2.b; c2 = c2.b) {
    $author$project$Types$w3e_Rec(c2.a);
  }
  _W3_int(_W3_dictSize(v.set.a));
  var st1 = [], nd1 = v.set.a;
  for (;;) {
    while (nd1.$ === _W3_T_RBNODE) { st1.push(nd1); nd1 = nd1.d; }
    if (st1.length === 0) break;
    nd1 = st1.pop();
    _W3_str(nd1.b);
    nd1 = nd1.e;
  }
  _W3_int($elm$time$Time$posixToMillis(v.time));
  _W3_u8(v.triple.a ? 1 : 0);
  _W3_f64(v.triple.b);
  $author$project$Types$w3e_Ctors(v.triple.c);
  _W3_int(v.zTuple.a);
  _W3_str(v.zTuple.b);
function $author$project$Types$w3e_Cross(v) {
  $author$project$Other$w3e_OtherCtor(v.ctor);
  $author$project$Other$w3e_Opaque(v.opaque);
  $author$project$Other$w3e_OtherRec(v.other);
  $author$project$Types$w3e_Tree(v.tree, $author$project$Types$w3ea_Cross_2);
  $author$project$Types$w3e_Wrapped(v.wrapped, $author$project$Types$w3ea_Cross_3);
function $author$project$Types$w3e_Ctors(v) {
  switch (v.$) {
    case 'A':
      _W3_u8(0);
      return;
    case 'AB':
      _W3_u8(1);
      return;
    case 'A_b':
      _W3_u8(2);
      return;
    case 'Aaa':
      _W3_u8(3);
      return;
    case 'Ab':
      _W3_u8(4);
      _W3_str(v.a);
      _W3_f64(v.b);
      return;
    case 'B2':
      _W3_u8(5);
      _W3_int(v.a);
      return;
    case 'Ba':
      _W3_u8(6);
      var m1 = v.a;
      if (m1.$ === 'Nothing') { _W3_u8(0); } else {
        _W3_u8(1);
        $author$project$Types$w3e_Ctors(m1.a);
      }
      return;
    case 'LongerName':
      _W3_u8(7);
      return;
    case 'Mid':
      _W3_u8(8);
      return;
    case 'Zz':
      _W3_u8(9);
      return;
  }
function $author$project$Types$w3e_Forest(v) {
  _W3_u8(0);
  var n2 = 0;
  for (var c2 = v.a.kids; c2.b; c2 = c2.b) n2++;
  _W3_int(n2);
  for (var c2 = v.a.kids; c2.b; c2 = c2.b) {
    $author$project$Types$w3e_Forest(c2.a);
  }
  _W3_str(v.a.nm);
  _W3_int(v.a.z);
function $author$project$Types$w3e_FrontendModel(v) {
  $author$project$Types$w3e_Ctors(v.a);
  _W3_int(v.key);
  $author$project$Types$w3e_Nested(v.nested);
  $author$project$Types$w3e_Rec(v.zeta);
function $author$project$Types$w3e_FrontendMsg(v) {
  switch (v.$) {
    case 'F':
      _W3_u8(0);
      $author$project$Types$w3e_Choice(v.a, $author$project$Types$w3e_Ctors, $author$project$Types$w3e_Nested);
      return;
    case 'FNoop':
      _W3_u8(1);
      return;
    case 'FaMsg':
      _W3_u8(2);
      return;
    case 'FzMsg':
      _W3_u8(3);
      $author$project$Types$w3e_Rec(v.a);
      return;
  }
function $author$project$Types$w3e_Nested(v) {
  var n1 = 0;
  for (var c1 = v.list; c1.b; c1 = c1.b) n1++;
  _W3_int(n1);
  for (var c1 = v.list; c1.b; c1 = c1.b) {
    _W3_str(c1.a.pp);
    _W3_int(c1.a.q);
    _W3_u8(c1.a.r ? 1 : 0);
  }
  _W3_int(v.outer.aInner.x);
  _W3_f64(v.outer.aInner.xLonger);
  _W3_str(v.outer.aInner.yy);
  _W3_int(v.outer.zInner);
  $author$project$Types$w3e_Rec(v.rec);
function $author$project$Types$w3e_Rec(v) {
  _W3_str(v.a);
  var n1 = 0;
  for (var c1 = v.ab; c1.b; c1 = c1.b) n1++;
  _W3_int(n1);
  for (var c1 = v.ab; c1.b; c1 = c1.b) {
    _W3_int(c1.a);
  }
  _W3_str(v.b1);
  _W3_int(_Char_toCode(v.b2));
  _W3_u8(v.bA.$ === 'LT' ? 0 : v.bA.$ === 'EQ' ? 1 : 2);
  _W3_int(v.b_);
  _W3_u8(v.longerName ? 1 : 0);
  _W3_f64(v.mid);
  _W3_int(v.zeta);
  var m1 = v.zz;
  if (m1.$ === 'Nothing') { _W3_u8(0); } else {
    _W3_u8(1);
    _W3_int(m1.a);
  }
function $author$project$Types$w3e_ToBackend(v) {
  switch (v.$) {
    case 'Tb':
      _W3_u8(0);
      return;
    case 'Tb2':
      _W3_u8(1);
      $author$project$Types$w3e_UsesExt(v.a);
      return;
    case 'ToBackendA':
      _W3_u8(2);
      $author$project$Types$w3e_Choice(v.a, $author$project$Types$w3ea_ToBackend_0, $author$project$Types$w3ea_ToBackend_1);
      return;
    case 'ToBackendZ':
      _W3_u8(3);
      $author$project$Types$w3e_Ctors(v.a);
      return;
  }
function $author$project$Types$w3e_ToFrontend(v) {
  switch (v.$) {
    case 'T':
      _W3_u8(0);
      $author$project$Types$w3e_Containers(v.a);
      return;
    case 'Tf':
      _W3_u8(1);
      return;
    case 'TfLonger':
      _W3_u8(2);
      $author$project$Types$w3e_Nested(v.a);
      return;
  }
function $author$project$Types$w3e_Tree(v, e0) {
  switch (v.$) {
    case 'Leaf':
      _W3_u8(0);
      return;
    case 'Node':
      _W3_u8(1);
      $author$project$Types$w3e_Tree(v.a, e0);
      e0(v.b);
      $author$project$Types$w3e_Tree(v.c, e0);
      return;
  }
function $author$project$Types$w3e_UsesExt(v) {
  _W3_int(v.e.k);
  _W3_u8(v.e.kk ? 1 : 0);
  var m2 = v.e.m;
  if (m2.$ === 'Nothing') { _W3_u8(0); } else {
    _W3_u8(1);
    _W3_str(m2.a);
  }
  var n2 = 0;
  for (var c2 = v.e.zz; c2.b; c2 = c2.b) n2++;
  _W3_int(n2);
  for (var c2 = v.e.zz; c2.b; c2 = c2.b) {
    _W3_int(c2.a);
  }
  $author$project$Other$w3e_Person(v.p);
  _W3_f64(v.q.a);
  _W3_int(v.q.ex);
  _W3_str(v.q.extra);
  _W3_str(v.q.name);
  _W3_int(v.q.zid);
function $author$project$Types$w3e_Wrapped(v, e0) {
  var n1 = 0;
  for (var c1 = v.at; c1.b; c1 = c1.b) n1++;
  _W3_int(n1);
  for (var c1 = v.at; c1.b; c1 = c1.b) {
    e0(c1.a);
  }
  e0(v.value);
  _W3_int(v.zcount);
var $author$project$Other$w3_decode_Opaque = _W3_decWrap($author$project$Other$w3d_Opaque);
var $author$project$Other$w3_decode_OtherCtor = _W3_decWrap($author$project$Other$w3d_OtherCtor);
var $author$project$Other$w3_decode_OtherRec = _W3_decWrap($author$project$Other$w3d_OtherRec);
var $author$project$Other$w3_decode_OtherRec2 = _W3_decWrap($author$project$Other$w3d_OtherRec2);
var $author$project$Other$w3_decode_Person = _W3_decWrap($author$project$Other$w3d_Person);
var $author$project$Other$w3_encode_Opaque = _W3_encWrap($author$project$Other$w3e_Opaque);
var $author$project$Other$w3_encode_OtherCtor = _W3_encWrap($author$project$Other$w3e_OtherCtor);
var $author$project$Other$w3_encode_OtherRec = _W3_encWrap($author$project$Other$w3e_OtherRec);
var $author$project$Other$w3_encode_OtherRec2 = _W3_encWrap($author$project$Other$w3e_OtherRec2);
var $author$project$Other$w3_encode_Person = _W3_encWrap($author$project$Other$w3e_Person);
var $author$project$Types$w3_decode_BackendModel = _W3_decWrap($author$project$Types$w3d_BackendModel);
var $author$project$Types$w3_decode_BackendMsg = _W3_decWrap($author$project$Types$w3d_BackendMsg);
var $author$project$Types$w3_decode_Choice = function (a0) { return function (a1) { var d0 = _W3_toDirectDec(a0), d1 = _W3_toDirectDec(a1); return _W3_decWrap(function () { return $author$project$Types$w3d_Choice(d0, d1); }); }; };
var $author$project$Types$w3_decode_Containers = _W3_decWrap($author$project$Types$w3d_Containers);
var $author$project$Types$w3_decode_Cross = _W3_decWrap($author$project$Types$w3d_Cross);
var $author$project$Types$w3_decode_Ctors = _W3_decWrap($author$project$Types$w3d_Ctors);
var $author$project$Types$w3_decode_Forest = _W3_decWrap($author$project$Types$w3d_Forest);
var $author$project$Types$w3_decode_FrontendModel = _W3_decWrap($author$project$Types$w3d_FrontendModel);
var $author$project$Types$w3_decode_FrontendMsg = _W3_decWrap($author$project$Types$w3d_FrontendMsg);
var $author$project$Types$w3_decode_Nested = _W3_decWrap($author$project$Types$w3d_Nested);
var $author$project$Types$w3_decode_Rec = _W3_decWrap($author$project$Types$w3d_Rec);
var $author$project$Types$w3_decode_ToBackend = _W3_decWrap($author$project$Types$w3d_ToBackend);
var $author$project$Types$w3_decode_ToFrontend = _W3_decWrap($author$project$Types$w3d_ToFrontend);
var $author$project$Types$w3_decode_Tree = function (a0) { var d0 = _W3_toDirectDec(a0); return _W3_decWrap(function () { return $author$project$Types$w3d_Tree(d0); }); };
var $author$project$Types$w3_decode_UsesExt = _W3_decWrap($author$project$Types$w3d_UsesExt);
var $author$project$Types$w3_decode_Wrapped = function (a0) { var d0 = _W3_toDirectDec(a0); return _W3_decWrap(function () { return $author$project$Types$w3d_Wrapped(d0); }); };
var $author$project$Types$w3_encode_BackendModel = _W3_encWrap($author$project$Types$w3e_BackendModel);
var $author$project$Types$w3_encode_BackendMsg = _W3_encWrap($author$project$Types$w3e_BackendMsg);
var $author$project$Types$w3_encode_Choice = function (a0) { return function (a1) { return (function () { var e0 = _W3_toDirectEnc(a0), e1 = _W3_toDirectEnc(a1); return _W3_encWrap(function (v) { $author$project$Types$w3e_Choice(v, e0, e1); }); })(); }; };
var $author$project$Types$w3_encode_Containers = _W3_encWrap($author$project$Types$w3e_Containers);
var $author$project$Types$w3_encode_Cross = _W3_encWrap($author$project$Types$w3e_Cross);
var $author$project$Types$w3_encode_Ctors = _W3_encWrap($author$project$Types$w3e_Ctors);
var $author$project$Types$w3_encode_Forest = _W3_encWrap($author$project$Types$w3e_Forest);
var $author$project$Types$w3_encode_FrontendModel = _W3_encWrap($author$project$Types$w3e_FrontendModel);
var $author$project$Types$w3_encode_FrontendMsg = _W3_encWrap($author$project$Types$w3e_FrontendMsg);
var $author$project$Types$w3_encode_Nested = _W3_encWrap($author$project$Types$w3e_Nested);
var $author$project$Types$w3_encode_Rec = _W3_encWrap($author$project$Types$w3e_Rec);
var $author$project$Types$w3_encode_ToBackend = _W3_encWrap($author$project$Types$w3e_ToBackend);
var $author$project$Types$w3_encode_ToFrontend = _W3_encWrap($author$project$Types$w3e_ToFrontend);
var $author$project$Types$w3_encode_Tree = function (a0) { return (function () { var e0 = _W3_toDirectEnc(a0); return _W3_encWrap(function (v) { $author$project$Types$w3e_Tree(v, e0); }); })(); };
var $author$project$Types$w3_encode_UsesExt = _W3_encWrap($author$project$Types$w3e_UsesExt);
var $author$project$Types$w3_encode_Wrapped = function (a0) { return (function () { var e0 = _W3_toDirectEnc(a0); return _W3_encWrap(function (v) { $author$project$Types$w3e_Wrapped(v, e0); }); })(); };
var $author$project$Types$w3da_BackendModel_0 = function () { return _W3_rint(); };
var $author$project$Types$w3da_BackendModel_1 = function () { return _W3_rstr(); };
var $author$project$Types$w3da_Cross_0 = function () { return _W3_rint(); };
var $author$project$Types$w3da_Cross_1 = function () { return _W3_rstr(); };
var $author$project$Types$w3da_Cross_2 = function () { return $author$project$Types$w3d_Choice($author$project$Types$w3da_Cross_0, $author$project$Types$w3da_Cross_1); };
var $author$project$Types$w3da_Cross_3 = function () { return $author$project$Types$w3d_Choice($author$project$Types$w3d_Rec, $author$project$Types$w3d_Ctors); };
var $author$project$Types$w3da_ToBackend_0 = function () { return _W3_rstr(); };
var $author$project$Types$w3da_ToBackend_1 = function () { return _W3_rint(); };
var $author$project$Types$w3ea_BackendModel_0 = function (x) {
  _W3_int(x);
var $author$project$Types$w3ea_BackendModel_1 = function (x) {
  _W3_str(x);
var $author$project$Types$w3ea_Cross_0 = function (x) {
  _W3_int(x);
var $author$project$Types$w3ea_Cross_1 = function (x) {
  _W3_str(x);
var $author$project$Types$w3ea_Cross_2 = function (x) {
  $author$project$Types$w3e_Choice(x, $author$project$Types$w3ea_Cross_0, $author$project$Types$w3ea_Cross_1);
var $author$project$Types$w3ea_Cross_3 = function (x) {
  $author$project$Types$w3e_Choice(x, $author$project$Types$w3e_Rec, $author$project$Types$w3e_Ctors);
var $author$project$Types$w3ea_ToBackend_0 = function (x) {
  _W3_str(x);
var $author$project$Types$w3ea_ToBackend_1 = function (x) {
  _W3_int(x);
