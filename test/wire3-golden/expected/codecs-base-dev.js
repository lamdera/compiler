var $author$project$Other$w3_decode_Opaque = A2(
	$lamdera$codecs$Lamdera$Wire3$andThenDecode,
	function (w3v) {
		if (!w3v) {
			return $lamdera$codecs$Lamdera$Wire3$andMapDecode$(
				$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
					$lamdera$codecs$Lamdera$Wire3$decodeInt,
					$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
						$lamdera$codecs$Lamdera$Wire3$decodeString,
						$lamdera$codecs$Lamdera$Wire3$succeedDecode(
							F2(
								function (a0, inner0) {
									return {a: a0, inner: inner0};
								})))),
				$lamdera$codecs$Lamdera$Wire3$succeedDecode($author$project$Other$Opaque));
		} else {
			return $lamdera$codecs$Lamdera$Wire3$failDecode;
		}
	},
	$elm$bytes$Bytes$Decode$unsignedInt8);
var $author$project$Other$w3_decode_OtherRec = $lamdera$codecs$Lamdera$Wire3$andMapDecode$(
	$author$project$Other$w3_decode_OtherCtor,
	$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
		$lamdera$codecs$Lamdera$Wire3$decodePair$($lamdera$codecs$Lamdera$Wire3$decodeInt, $lamdera$codecs$Lamdera$Wire3$decodeFloat),
		$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
			$lamdera$codecs$Lamdera$Wire3$decodeDict$($lamdera$codecs$Lamdera$Wire3$decodeString, $lamdera$codecs$Lamdera$Wire3$decodeInt),
			$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
				$lamdera$codecs$Lamdera$Wire3$decodeMaybe($author$project$Other$w3_decode_OtherRec2),
				$lamdera$codecs$Lamdera$Wire3$succeedDecode(
					F4(
						function (a0, ab0, abc0, zebra0) {
							return {a: a0, ab: ab0, abc: abc0, zebra: zebra0};
						}))))));
var $author$project$Other$w3_decode_OtherRec2 = $lamdera$codecs$Lamdera$Wire3$andMapDecode$(
	$lamdera$codecs$Lamdera$Wire3$decodeInt,
	$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
		$lamdera$codecs$Lamdera$Wire3$decodeString,
		$lamdera$codecs$Lamdera$Wire3$succeedDecode(
			F2(
				function (x0, yy0) {
					return {x: x0, yy: yy0};
				}))));
var $author$project$Other$w3_decode_Person = $lamdera$codecs$Lamdera$Wire3$andMapDecode$(
	$lamdera$codecs$Lamdera$Wire3$decodeInt,
	$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
		$lamdera$codecs$Lamdera$Wire3$decodeString,
		$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
			$lamdera$codecs$Lamdera$Wire3$decodeString,
			$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
				$lamdera$codecs$Lamdera$Wire3$decodeBool,
				$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
					$lamdera$codecs$Lamdera$Wire3$decodeInt,
					$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
						$lamdera$codecs$Lamdera$Wire3$decodeFloat,
						$lamdera$codecs$Lamdera$Wire3$succeedDecode(
							F6(
								function (a0, age0, b0, longest0, name0, zid0) {
									return {a: a0, age: age0, b: b0, longest: longest0, name: name0, zid: zid0};
								}))))))));
var $author$project$Other$w3_encode_Opaque = function (w3v) {
	var v0 = w3v.a;
	return $lamdera$codecs$Lamdera$Wire3$encodeSequenceWithoutLength(
		_List_fromArray(
			[
				$elm$bytes$Bytes$Encode$unsignedInt8(0),
				function (w3_rec_var0) {
				return $lamdera$codecs$Lamdera$Wire3$encodeSequenceWithoutLength(
					_List_fromArray(
						[
							$lamdera$codecs$Lamdera$Wire3$encodeString(w3_rec_var0.a),
							$lamdera$codecs$Lamdera$Wire3$encodeInt(w3_rec_var0.inner)
						]));
			}(v0)
			]));
var $author$project$Other$w3_encode_OtherCtor = function (w3v) {
	switch (w3v.$) {
		case 'Al':
			var v0 = w3v.a;
			return $lamdera$codecs$Lamdera$Wire3$encodeSequenceWithoutLength(
				_List_fromArray(
					[
						$elm$bytes$Bytes$Encode$unsignedInt8(0),
						$lamdera$codecs$Lamdera$Wire3$encodeInt(v0)
					]));
		case 'B':
			return $elm$bytes$Bytes$Encode$unsignedInt8(1);
		case 'Bb':
			var v0 = w3v.a;
			return $lamdera$codecs$Lamdera$Wire3$encodeSequenceWithoutLength(
				_List_fromArray(
					[
						$elm$bytes$Bytes$Encode$unsignedInt8(2),
						$lamdera$codecs$Lamdera$Wire3$encodeList$($author$project$Other$w3_encode_OtherCtor, v0)
					]));
		case 'MiddleOne':
			var v0 = w3v.a;
			var v1 = w3v.b;
			return $lamdera$codecs$Lamdera$Wire3$encodeSequenceWithoutLength(
				_List_fromArray(
					[
						$elm$bytes$Bytes$Encode$unsignedInt8(3),
						$lamdera$codecs$Lamdera$Wire3$encodeString(v0),
						$lamdera$codecs$Lamdera$Wire3$encodeBool(v1)
					]));
		default:
			return $elm$bytes$Bytes$Encode$unsignedInt8(4);
	}
var $author$project$Other$w3_encode_OtherRec = function (w3_rec_var0) {
	return $lamdera$codecs$Lamdera$Wire3$encodeSequenceWithoutLength(
		_List_fromArray(
			[
				$lamdera$codecs$Lamdera$Wire3$encodeMaybe$($author$project$Other$w3_encode_OtherRec2, w3_rec_var0.a),
				$lamdera$codecs$Lamdera$Wire3$encodeDict$($lamdera$codecs$Lamdera$Wire3$encodeString, $lamdera$codecs$Lamdera$Wire3$encodeInt, w3_rec_var0.ab),
				$lamdera$codecs$Lamdera$Wire3$encodePair$($lamdera$codecs$Lamdera$Wire3$encodeInt, $lamdera$codecs$Lamdera$Wire3$encodeFloat, w3_rec_var0.abc),
				$author$project$Other$w3_encode_OtherCtor(w3_rec_var0.zebra)
			]));
var $author$project$Other$w3_encode_OtherRec2 = function (w3_rec_var0) {
	return $lamdera$codecs$Lamdera$Wire3$encodeSequenceWithoutLength(
		_List_fromArray(
			[
				$lamdera$codecs$Lamdera$Wire3$encodeString(w3_rec_var0.x),
				$lamdera$codecs$Lamdera$Wire3$encodeInt(w3_rec_var0.yy)
			]));
var $author$project$Other$w3_encode_Person = function (w3_rec_var0) {
	return $lamdera$codecs$Lamdera$Wire3$encodeSequenceWithoutLength(
		_List_fromArray(
			[
				$lamdera$codecs$Lamdera$Wire3$encodeFloat(w3_rec_var0.a),
				$lamdera$codecs$Lamdera$Wire3$encodeInt(w3_rec_var0.age),
				$lamdera$codecs$Lamdera$Wire3$encodeBool(w3_rec_var0.b),
				$lamdera$codecs$Lamdera$Wire3$encodeString(w3_rec_var0.longest),
				$lamdera$codecs$Lamdera$Wire3$encodeString(w3_rec_var0.name),
				$lamdera$codecs$Lamdera$Wire3$encodeInt(w3_rec_var0.zid)
			]));
var $author$project$Types$w3_decode_BackendModel = $lamdera$codecs$Lamdera$Wire3$andMapDecode$(
	$author$project$Types$w3_decode_UsesExt,
	$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
		$lamdera$codecs$Lamdera$Wire3$decodeList(
			$author$project$Types$w3_decode_Tree($author$project$Types$w3_decode_Ctors)),
		$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
			$author$project$Types$w3_decode_Forest,
			$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
				$author$project$Types$w3_decode_Cross,
				$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
					$author$project$Types$w3_decode_Containers,
					$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
						$author$project$Types$w3_decode_Choice$($lamdera$codecs$Lamdera$Wire3$decodeInt, $lamdera$codecs$Lamdera$Wire3$decodeString),
						$lamdera$codecs$Lamdera$Wire3$succeedDecode(
							F6(
								function (choice0, containers0, cross0, forest0, trees0, usesExt0) {
									return {choice: choice0, containers: containers0, cross: cross0, forest: forest0, trees: trees0, usesExt: usesExt0};
								}))))))));
var $author$project$Types$w3_decode_BackendMsg = A2(
	$lamdera$codecs$Lamdera$Wire3$andThenDecode,
	function (w3v) {
		switch (w3v) {
			case 0:
				return $lamdera$codecs$Lamdera$Wire3$succeedDecode($author$project$Types$BNoop);
			case 1:
				return $lamdera$codecs$Lamdera$Wire3$andMapDecode$(
					$author$project$Types$w3_decode_Cross,
					$lamdera$codecs$Lamdera$Wire3$succeedDecode($author$project$Types$Bb_));
			case 2:
				return $lamdera$codecs$Lamdera$Wire3$andMapDecode$(
					$lamdera$codecs$Lamdera$Wire3$decodeInt,
					$lamdera$codecs$Lamdera$Wire3$succeedDecode($author$project$Types$Bz));
			default:
				return $lamdera$codecs$Lamdera$Wire3$failDecode;
		}
	},
	$elm$bytes$Bytes$Decode$unsignedInt8);
var $author$project$Types$w3_decode_Choice = F2($author$project$Types$w3_decode_Choice$);
var $author$project$Types$w3_decode_Choice$ = function (w3_x_c_a, w3_x_c_b) {
	return A2(
		$lamdera$codecs$Lamdera$Wire3$andThenDecode,
		function (w3v) {
			switch (w3v) {
				case 0:
					return $lamdera$codecs$Lamdera$Wire3$andMapDecode$(
						w3_x_c_b,
						$lamdera$codecs$Lamdera$Wire3$succeedDecode($author$project$Types$Al));
				case 1:
					return $lamdera$codecs$Lamdera$Wire3$andMapDecode$(
						w3_x_c_b,
						$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
							w3_x_c_a,
							$lamdera$codecs$Lamdera$Wire3$succeedDecode($author$project$Types$Middle)));
				case 2:
					return $lamdera$codecs$Lamdera$Wire3$succeedDecode($author$project$Types$None_);
				case 3:
					return $lamdera$codecs$Lamdera$Wire3$andMapDecode$(
						w3_x_c_a,
						$lamdera$codecs$Lamdera$Wire3$succeedDecode($author$project$Types$Zed));
				default:
					return $lamdera$codecs$Lamdera$Wire3$failDecode;
			}
		},
		$elm$bytes$Bytes$Decode$unsignedInt8);
var $author$project$Types$w3_decode_Containers = $lamdera$codecs$Lamdera$Wire3$andMapDecode$(
	$lamdera$codecs$Lamdera$Wire3$decodePair$($lamdera$codecs$Lamdera$Wire3$decodeInt, $lamdera$codecs$Lamdera$Wire3$decodeString),
	$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
		$lamdera$codecs$Lamdera$Wire3$decodeUnit,
		$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
			$lamdera$codecs$Lamdera$Wire3$decodeTriple$($lamdera$codecs$Lamdera$Wire3$decodeBool, $lamdera$codecs$Lamdera$Wire3$decodeFloat, $author$project$Types$w3_decode_Ctors),
			$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
				A2(
					$lamdera$codecs$Lamdera$Wire3$andThenDecode,
					function (t) {
						return $lamdera$codecs$Lamdera$Wire3$succeedDecode(
							$elm$time$Time$millisToPosix(t));
					},
					$lamdera$codecs$Lamdera$Wire3$decodeInt),
				$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
					$lamdera$codecs$Lamdera$Wire3$decodeSet($lamdera$codecs$Lamdera$Wire3$decodeString),
					$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
						$lamdera$containers$SeqSet$decodeSet($author$project$Types$w3_decode_Rec),
						$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
							$lamdera$containers$SeqSet$decodeSet($author$project$Types$w3_decode_Ctors),
							$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
								$lamdera$containers$SeqDict$decodeDict$(
									$author$project$Types$w3_decode_Ctors,
									$lamdera$codecs$Lamdera$Wire3$decodeList($author$project$Types$w3_decode_Ctors)),
								$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
									$lamdera$containers$SeqDict$decodeDict$($author$project$Types$w3_decode_Rec, $lamdera$codecs$Lamdera$Wire3$decodeInt),
									$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
										$lamdera$codecs$Lamdera$Wire3$decodeResult$(
											$lamdera$codecs$Lamdera$Wire3$decodeInt,
											$lamdera$codecs$Lamdera$Wire3$decodeList($author$project$Types$w3_decode_Ctors)),
										$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
											$lamdera$codecs$Lamdera$Wire3$decodeResult$($lamdera$codecs$Lamdera$Wire3$decodeString, $author$project$Types$w3_decode_Ctors),
											$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
												$lamdera$codecs$Lamdera$Wire3$decodeOrder,
												$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
													$lamdera$codecs$Lamdera$Wire3$decodeMaybe($author$project$Types$w3_decode_Rec),
													$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
														$lamdera$codecs$Lamdera$Wire3$decodeList($author$project$Types$w3_decode_Rec),
														$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
															$lamdera$codecs$Lamdera$Wire3$decodeDict$(
																$lamdera$codecs$Lamdera$Wire3$decodePair$($lamdera$codecs$Lamdera$Wire3$decodeInt, $lamdera$codecs$Lamdera$Wire3$decodeString),
																$lamdera$codecs$Lamdera$Wire3$decodeFloat),
															$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
																$lamdera$codecs$Lamdera$Wire3$decodeDict$($lamdera$codecs$Lamdera$Wire3$decodeString, $author$project$Types$w3_decode_Rec),
																$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
																	$lamdera$codecs$Lamdera$Wire3$decodeBytes,
																	$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
																		$lamdera$codecs$Lamdera$Wire3$decodeArray($author$project$Types$w3_decode_Ctors),
																		$lamdera$codecs$Lamdera$Wire3$succeedDecode(
																			function (array0) {
																				return function (bytes0) {
																					return function (dict0) {
																						return function (dictTuple0) {
																							return function (list0) {
																								return function (maybe0) {
																									return function (order0) {
																										return function (result0) {
																											return function (resultOk0) {
																												return function (seqDict0) {
																													return function (seqDictCtor0) {
																														return function (seqSet0) {
																															return function (seqSetRec0) {
																																return function (set0) {
																																	return function (time0) {
																																		return function (triple0) {
																																			return function (unit0) {
																																				return function (zTuple0) {
																																					return {array: array0, bytes: bytes0, dict: dict0, dictTuple: dictTuple0, list: list0, maybe: maybe0, order: order0, result: result0, resultOk: resultOk0, seqDict: seqDict0, seqDictCtor: seqDictCtor0, seqSet: seqSet0, seqSetRec: seqSetRec0, set: set0, time: time0, triple: triple0, unit: unit0, zTuple: zTuple0};
																																				};
																																			};
																																		};
																																	};
																																};
																															};
																														};
																													};
																												};
																											};
																										};
																									};
																								};
																							};
																						};
																					};
																				};
																			})))))))))))))))))));
var $author$project$Types$w3_decode_Cross = $lamdera$codecs$Lamdera$Wire3$andMapDecode$(
	$author$project$Types$w3_decode_Wrapped(
		$author$project$Types$w3_decode_Choice$($author$project$Types$w3_decode_Rec, $author$project$Types$w3_decode_Ctors)),
	$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
		$author$project$Types$w3_decode_Tree(
			$author$project$Types$w3_decode_Choice$($lamdera$codecs$Lamdera$Wire3$decodeInt, $lamdera$codecs$Lamdera$Wire3$decodeString)),
		$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
			$author$project$Other$w3_decode_OtherRec,
			$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
				$author$project$Other$w3_decode_Opaque,
				$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
					$author$project$Other$w3_decode_OtherCtor,
					$lamdera$codecs$Lamdera$Wire3$succeedDecode(
						F5(
							function (ctor0, opaque0, other0, tree0, wrapped0) {
								return {ctor: ctor0, opaque: opaque0, other: other0, tree: tree0, wrapped: wrapped0};
							})))))));
var $author$project$Types$w3_decode_FrontendModel = $lamdera$codecs$Lamdera$Wire3$andMapDecode$(
	$author$project$Types$w3_decode_Rec,
	$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
		$author$project$Types$w3_decode_Nested,
		$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
			$lamdera$codecs$Lamdera$Wire3$decodeInt,
			$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
				$author$project$Types$w3_decode_Ctors,
				$lamdera$codecs$Lamdera$Wire3$succeedDecode(
					F4(
						function (a0, key0, nested0, zeta0) {
							return {a: a0, key: key0, nested: nested0, zeta: zeta0};
						}))))));
var $author$project$Types$w3_decode_FrontendMsg = A2(
	$lamdera$codecs$Lamdera$Wire3$andThenDecode,
	function (w3v) {
		switch (w3v) {
			case 0:
				return $lamdera$codecs$Lamdera$Wire3$andMapDecode$(
					$author$project$Types$w3_decode_Choice$($author$project$Types$w3_decode_Ctors, $author$project$Types$w3_decode_Nested),
					$lamdera$codecs$Lamdera$Wire3$succeedDecode($author$project$Types$F));
			case 1:
				return $lamdera$codecs$Lamdera$Wire3$succeedDecode($author$project$Types$FNoop);
			case 2:
				return $lamdera$codecs$Lamdera$Wire3$succeedDecode($author$project$Types$FaMsg);
			case 3:
				return $lamdera$codecs$Lamdera$Wire3$andMapDecode$(
					$author$project$Types$w3_decode_Rec,
					$lamdera$codecs$Lamdera$Wire3$succeedDecode($author$project$Types$FzMsg));
			default:
				return $lamdera$codecs$Lamdera$Wire3$failDecode;
		}
	},
	$elm$bytes$Bytes$Decode$unsignedInt8);
var $author$project$Types$w3_decode_Nested = $lamdera$codecs$Lamdera$Wire3$andMapDecode$(
	$author$project$Types$w3_decode_Rec,
	$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
		$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
			$lamdera$codecs$Lamdera$Wire3$decodeInt,
			$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
				$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
					$lamdera$codecs$Lamdera$Wire3$decodeString,
					$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
						$lamdera$codecs$Lamdera$Wire3$decodeFloat,
						$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
							$lamdera$codecs$Lamdera$Wire3$decodeInt,
							$lamdera$codecs$Lamdera$Wire3$succeedDecode(
								F3(
									function (x0, xLonger0, yy0) {
										return {x: x0, xLonger: xLonger0, yy: yy0};
									}))))),
				$lamdera$codecs$Lamdera$Wire3$succeedDecode(
					F2(
						function (aInner0, zInner0) {
							return {aInner: aInner0, zInner: zInner0};
						})))),
		$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
			$lamdera$codecs$Lamdera$Wire3$decodeList(
				$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
					$lamdera$codecs$Lamdera$Wire3$decodeBool,
					$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
						$lamdera$codecs$Lamdera$Wire3$decodeInt,
						$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
							$lamdera$codecs$Lamdera$Wire3$decodeString,
							$lamdera$codecs$Lamdera$Wire3$succeedDecode(
								F3(
									function (pp0, q0, r0) {
										return {pp: pp0, q: q0, r: r0};
									})))))),
			$lamdera$codecs$Lamdera$Wire3$succeedDecode(
				F3(
					function (list0, outer0, rec0) {
						return {list: list0, outer: outer0, rec: rec0};
					})))));
var $author$project$Types$w3_decode_Rec = $lamdera$codecs$Lamdera$Wire3$andMapDecode$(
	$lamdera$codecs$Lamdera$Wire3$decodeMaybe($lamdera$codecs$Lamdera$Wire3$decodeInt),
	$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
		$lamdera$codecs$Lamdera$Wire3$decodeInt,
		$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
			$lamdera$codecs$Lamdera$Wire3$decodeFloat,
			$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
				$lamdera$codecs$Lamdera$Wire3$decodeBool,
				$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
					$lamdera$codecs$Lamdera$Wire3$decodeInt,
					$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
						$lamdera$codecs$Lamdera$Wire3$decodeOrder,
						$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
							$lamdera$codecs$Lamdera$Wire3$decodeChar,
							$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
								$lamdera$codecs$Lamdera$Wire3$decodeString,
								$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
									$lamdera$codecs$Lamdera$Wire3$decodeList($lamdera$codecs$Lamdera$Wire3$decodeInt),
									$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
										$lamdera$codecs$Lamdera$Wire3$decodeUnit,
										$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
											$lamdera$codecs$Lamdera$Wire3$decodeString,
											$lamdera$codecs$Lamdera$Wire3$succeedDecode(
												function (a0) {
													return function (aaa0) {
														return function (ab0) {
															return function (b10) {
																return function (b20) {
																	return function (bA0) {
																		return function (b_0) {
																			return function (longerName0) {
																				return function (mid0) {
																					return function (zeta0) {
																						return function (zz0) {
																							return {a: a0, aaa: aaa0, ab: ab0, b1: b10, b2: b20, bA: bA0, b_: b_0, longerName: longerName0, mid: mid0, zeta: zeta0, zz: zz0};
																						};
																					};
																				};
																			};
																		};
																	};
																};
															};
														};
													};
												}))))))))))));
var $author$project$Types$w3_decode_ToBackend = A2(
	$lamdera$codecs$Lamdera$Wire3$andThenDecode,
	function (w3v) {
		switch (w3v) {
			case 0:
				return $lamdera$codecs$Lamdera$Wire3$succeedDecode($author$project$Types$Tb);
			case 1:
				return $lamdera$codecs$Lamdera$Wire3$andMapDecode$(
					$author$project$Types$w3_decode_UsesExt,
					$lamdera$codecs$Lamdera$Wire3$succeedDecode($author$project$Types$Tb2));
			case 2:
				return $lamdera$codecs$Lamdera$Wire3$andMapDecode$(
					$author$project$Types$w3_decode_Choice$($lamdera$codecs$Lamdera$Wire3$decodeString, $lamdera$codecs$Lamdera$Wire3$decodeInt),
					$lamdera$codecs$Lamdera$Wire3$succeedDecode($author$project$Types$ToBackendA));
			case 3:
				return $lamdera$codecs$Lamdera$Wire3$andMapDecode$(
					$author$project$Types$w3_decode_Ctors,
					$lamdera$codecs$Lamdera$Wire3$succeedDecode($author$project$Types$ToBackendZ));
			default:
				return $lamdera$codecs$Lamdera$Wire3$failDecode;
		}
	},
	$elm$bytes$Bytes$Decode$unsignedInt8);
var $author$project$Types$w3_decode_ToFrontend = A2(
	$lamdera$codecs$Lamdera$Wire3$andThenDecode,
	function (w3v) {
		switch (w3v) {
			case 0:
				return $lamdera$codecs$Lamdera$Wire3$andMapDecode$(
					$author$project$Types$w3_decode_Containers,
					$lamdera$codecs$Lamdera$Wire3$succeedDecode($author$project$Types$T));
			case 1:
				return $lamdera$codecs$Lamdera$Wire3$succeedDecode($author$project$Types$Tf);
			case 2:
				return $lamdera$codecs$Lamdera$Wire3$andMapDecode$(
					$author$project$Types$w3_decode_Nested,
					$lamdera$codecs$Lamdera$Wire3$succeedDecode($author$project$Types$TfLonger));
			default:
				return $lamdera$codecs$Lamdera$Wire3$failDecode;
		}
	},
	$elm$bytes$Bytes$Decode$unsignedInt8);
var $author$project$Types$w3_decode_Tree = function (w3_x_c_a) {
	return A2(
		$lamdera$codecs$Lamdera$Wire3$andThenDecode,
		function (w3v) {
			switch (w3v) {
				case 0:
					return $lamdera$codecs$Lamdera$Wire3$succeedDecode($author$project$Types$Leaf);
				case 1:
					return $lamdera$codecs$Lamdera$Wire3$andMapDecode$(
						$author$project$Types$w3_decode_Tree(w3_x_c_a),
						$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
							w3_x_c_a,
							$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
								$author$project$Types$w3_decode_Tree(w3_x_c_a),
								$lamdera$codecs$Lamdera$Wire3$succeedDecode($author$project$Types$Node))));
				default:
					return $lamdera$codecs$Lamdera$Wire3$failDecode;
			}
		},
		$elm$bytes$Bytes$Decode$unsignedInt8);
var $author$project$Types$w3_decode_UsesExt = $lamdera$codecs$Lamdera$Wire3$andMapDecode$(
	$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
		$lamdera$codecs$Lamdera$Wire3$decodeInt,
		$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
			$lamdera$codecs$Lamdera$Wire3$decodeString,
			$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
				$lamdera$codecs$Lamdera$Wire3$decodeString,
				$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
					$lamdera$codecs$Lamdera$Wire3$decodeInt,
					$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
						$lamdera$codecs$Lamdera$Wire3$decodeFloat,
						$lamdera$codecs$Lamdera$Wire3$succeedDecode(
							F5(
								function (a0, ex0, extra0, name0, zid0) {
									return {a: a0, ex: ex0, extra: extra0, name: name0, zid: zid0};
								}))))))),
	$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
		$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
			$lamdera$codecs$Lamdera$Wire3$decodeInt,
			$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
				$lamdera$codecs$Lamdera$Wire3$decodeString,
				$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
					$lamdera$codecs$Lamdera$Wire3$decodeString,
					$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
						$lamdera$codecs$Lamdera$Wire3$decodeBool,
						$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
							$lamdera$codecs$Lamdera$Wire3$decodeInt,
							$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
								$lamdera$codecs$Lamdera$Wire3$decodeFloat,
								$lamdera$codecs$Lamdera$Wire3$succeedDecode(
									F6(
										function (a0, age0, b0, longest0, name0, zid0) {
											return {a: a0, age: age0, b: b0, longest: longest0, name: name0, zid: zid0};
										})))))))),
		$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
			$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
				$lamdera$codecs$Lamdera$Wire3$decodeList($lamdera$codecs$Lamdera$Wire3$decodeInt),
				$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
					$lamdera$codecs$Lamdera$Wire3$decodeMaybe($lamdera$codecs$Lamdera$Wire3$decodeString),
					$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
						$lamdera$codecs$Lamdera$Wire3$decodeBool,
						$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
							$lamdera$codecs$Lamdera$Wire3$decodeInt,
							$lamdera$codecs$Lamdera$Wire3$succeedDecode(
								F4(
									function (k0, kk0, m0, zz0) {
										return {k: k0, kk: kk0, m: m0, zz: zz0};
									})))))),
			$lamdera$codecs$Lamdera$Wire3$succeedDecode(
				F3(
					function (e0, p0, q0) {
						return {e: e0, p: p0, q: q0};
					})))));
var $author$project$Types$w3_decode_Wrapped = function (w3_x_c_a) {
	return $lamdera$codecs$Lamdera$Wire3$andMapDecode$(
		$lamdera$codecs$Lamdera$Wire3$decodeInt,
		$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
			w3_x_c_a,
			$lamdera$codecs$Lamdera$Wire3$andMapDecode$(
				$lamdera$codecs$Lamdera$Wire3$decodeList(w3_x_c_a),
				$lamdera$codecs$Lamdera$Wire3$succeedDecode(
					F3(
						function (at0, value0, zcount0) {
							return {at: at0, value: value0, zcount: zcount0};
						})))));
var $author$project$Types$w3_encode_BackendModel = function (w3_rec_var0) {
	return $lamdera$codecs$Lamdera$Wire3$encodeSequenceWithoutLength(
		_List_fromArray(
			[
				$author$project$Types$w3_encode_Choice$($lamdera$codecs$Lamdera$Wire3$encodeInt, $lamdera$codecs$Lamdera$Wire3$encodeString, w3_rec_var0.choice),
				$author$project$Types$w3_encode_Containers(w3_rec_var0.containers),
				$author$project$Types$w3_encode_Cross(w3_rec_var0.cross),
				$author$project$Types$w3_encode_Forest(w3_rec_var0.forest),
				$lamdera$codecs$Lamdera$Wire3$encodeList$(
				$author$project$Types$w3_encode_Tree($author$project$Types$w3_encode_Ctors),
				w3_rec_var0.trees),
				$author$project$Types$w3_encode_UsesExt(w3_rec_var0.usesExt)
			]));
var $author$project$Types$w3_encode_BackendMsg = function (w3v) {
	switch (w3v.$) {
		case 'BNoop':
			return $elm$bytes$Bytes$Encode$unsignedInt8(0);
		case 'Bb_':
			var v0 = w3v.a;
			return $lamdera$codecs$Lamdera$Wire3$encodeSequenceWithoutLength(
				_List_fromArray(
					[
						$elm$bytes$Bytes$Encode$unsignedInt8(1),
						$author$project$Types$w3_encode_Cross(v0)
					]));
		default:
			var v0 = w3v.a;
			return $lamdera$codecs$Lamdera$Wire3$encodeSequenceWithoutLength(
				_List_fromArray(
					[
						$elm$bytes$Bytes$Encode$unsignedInt8(2),
						$lamdera$codecs$Lamdera$Wire3$encodeInt(v0)
					]));
	}
var $author$project$Types$w3_encode_Choice = F3($author$project$Types$w3_encode_Choice$);
var $author$project$Types$w3_encode_Choice$ = function (w3_x_c_a, w3_x_c_b, w3v) {
	switch (w3v.$) {
		case 'Al':
			var v0 = w3v.a;
			return $lamdera$codecs$Lamdera$Wire3$encodeSequenceWithoutLength(
				_List_fromArray(
					[
						$elm$bytes$Bytes$Encode$unsignedInt8(0),
						w3_x_c_b(v0)
					]));
		case 'Middle':
			var v0 = w3v.a;
			var v1 = w3v.b;
			return $lamdera$codecs$Lamdera$Wire3$encodeSequenceWithoutLength(
				_List_fromArray(
					[
						$elm$bytes$Bytes$Encode$unsignedInt8(1),
						w3_x_c_a(v0),
						w3_x_c_b(v1)
					]));
		case 'None_':
			return $elm$bytes$Bytes$Encode$unsignedInt8(2);
		default:
			var v0 = w3v.a;
			return $lamdera$codecs$Lamdera$Wire3$encodeSequenceWithoutLength(
				_List_fromArray(
					[
						$elm$bytes$Bytes$Encode$unsignedInt8(3),
						w3_x_c_a(v0)
					]));
	}
var $author$project$Types$w3_encode_Containers = function (w3_rec_var0) {
	return $lamdera$codecs$Lamdera$Wire3$encodeSequenceWithoutLength(
		_List_fromArray(
			[
				$lamdera$codecs$Lamdera$Wire3$encodeArray$($author$project$Types$w3_encode_Ctors, w3_rec_var0.array),
				$lamdera$codecs$Lamdera$Wire3$encodeBytes(w3_rec_var0.bytes),
				$lamdera$codecs$Lamdera$Wire3$encodeDict$($lamdera$codecs$Lamdera$Wire3$encodeString, $author$project$Types$w3_encode_Rec, w3_rec_var0.dict),
				$lamdera$codecs$Lamdera$Wire3$encodeDict$(
				A2($lamdera$codecs$Lamdera$Wire3$encodePair, $lamdera$codecs$Lamdera$Wire3$encodeInt, $lamdera$codecs$Lamdera$Wire3$encodeString),
				$lamdera$codecs$Lamdera$Wire3$encodeFloat,
				w3_rec_var0.dictTuple),
				$lamdera$codecs$Lamdera$Wire3$encodeList$($author$project$Types$w3_encode_Rec, w3_rec_var0.list),
				$lamdera$codecs$Lamdera$Wire3$encodeMaybe$($author$project$Types$w3_encode_Rec, w3_rec_var0.maybe),
				$lamdera$codecs$Lamdera$Wire3$encodeOrder(w3_rec_var0.order),
				$lamdera$codecs$Lamdera$Wire3$encodeResult$($lamdera$codecs$Lamdera$Wire3$encodeString, $author$project$Types$w3_encode_Ctors, w3_rec_var0.result),
				$lamdera$codecs$Lamdera$Wire3$encodeResult$(
				$lamdera$codecs$Lamdera$Wire3$encodeInt,
				$lamdera$codecs$Lamdera$Wire3$encodeList($author$project$Types$w3_encode_Ctors),
				w3_rec_var0.resultOk),
				$lamdera$containers$SeqDict$encodeDict$($author$project$Types$w3_encode_Rec, $lamdera$codecs$Lamdera$Wire3$encodeInt, w3_rec_var0.seqDict),
				$lamdera$containers$SeqDict$encodeDict$(
				$author$project$Types$w3_encode_Ctors,
				$lamdera$codecs$Lamdera$Wire3$encodeList($author$project$Types$w3_encode_Ctors),
				w3_rec_var0.seqDictCtor),
				$lamdera$containers$SeqSet$encodeSet$($author$project$Types$w3_encode_Ctors, w3_rec_var0.seqSet),
				$lamdera$containers$SeqSet$encodeSet$($author$project$Types$w3_encode_Rec, w3_rec_var0.seqSetRec),
				$lamdera$codecs$Lamdera$Wire3$encodeSet$($lamdera$codecs$Lamdera$Wire3$encodeString, w3_rec_var0.set),
				function (t) {
				return $lamdera$codecs$Lamdera$Wire3$encodeInt(
					$elm$time$Time$posixToMillis(t));
			}(w3_rec_var0.time),
				$lamdera$codecs$Lamdera$Wire3$encodeTriple$($lamdera$codecs$Lamdera$Wire3$encodeBool, $lamdera$codecs$Lamdera$Wire3$encodeFloat, $author$project$Types$w3_encode_Ctors, w3_rec_var0.triple),
				$lamdera$codecs$Lamdera$Wire3$encodeUnit(w3_rec_var0.unit),
				$lamdera$codecs$Lamdera$Wire3$encodePair$($lamdera$codecs$Lamdera$Wire3$encodeInt, $lamdera$codecs$Lamdera$Wire3$encodeString, w3_rec_var0.zTuple)
			]));
var $author$project$Types$w3_encode_Cross = function (w3_rec_var0) {
	return $lamdera$codecs$Lamdera$Wire3$encodeSequenceWithoutLength(
		_List_fromArray(
			[
				$author$project$Other$w3_encode_OtherCtor(w3_rec_var0.ctor),
				$author$project$Other$w3_encode_Opaque(w3_rec_var0.opaque),
				$author$project$Other$w3_encode_OtherRec(w3_rec_var0.other),
				$author$project$Types$w3_encode_Tree$(
				A2($author$project$Types$w3_encode_Choice, $lamdera$codecs$Lamdera$Wire3$encodeInt, $lamdera$codecs$Lamdera$Wire3$encodeString),
				w3_rec_var0.tree),
				A2(
				$author$project$Types$w3_encode_Wrapped,
				A2($author$project$Types$w3_encode_Choice, $author$project$Types$w3_encode_Rec, $author$project$Types$w3_encode_Ctors),
				w3_rec_var0.wrapped)
			]));
var $author$project$Types$w3_encode_Ctors = function (w3v) {
	switch (w3v.$) {
		case 'A':
			return $elm$bytes$Bytes$Encode$unsignedInt8(0);
		case 'AB':
			return $elm$bytes$Bytes$Encode$unsignedInt8(1);
		case 'A_b':
			return $elm$bytes$Bytes$Encode$unsignedInt8(2);
		case 'Aaa':
			return $elm$bytes$Bytes$Encode$unsignedInt8(3);
		case 'Ab':
			var v0 = w3v.a;
			var v1 = w3v.b;
			return $lamdera$codecs$Lamdera$Wire3$encodeSequenceWithoutLength(
				_List_fromArray(
					[
						$elm$bytes$Bytes$Encode$unsignedInt8(4),
						$lamdera$codecs$Lamdera$Wire3$encodeString(v0),
						$lamdera$codecs$Lamdera$Wire3$encodeFloat(v1)
					]));
		case 'B2':
			var v0 = w3v.a;
			return $lamdera$codecs$Lamdera$Wire3$encodeSequenceWithoutLength(
				_List_fromArray(
					[
						$elm$bytes$Bytes$Encode$unsignedInt8(5),
						$lamdera$codecs$Lamdera$Wire3$encodeInt(v0)
					]));
		case 'Ba':
			var v0 = w3v.a;
			return $lamdera$codecs$Lamdera$Wire3$encodeSequenceWithoutLength(
				_List_fromArray(
					[
						$elm$bytes$Bytes$Encode$unsignedInt8(6),
						$lamdera$codecs$Lamdera$Wire3$encodeMaybe$($author$project$Types$w3_encode_Ctors, v0)
					]));
		case 'LongerName':
			return $elm$bytes$Bytes$Encode$unsignedInt8(7);
		case 'Mid':
			return $elm$bytes$Bytes$Encode$unsignedInt8(8);
		default:
			return $elm$bytes$Bytes$Encode$unsignedInt8(9);
	}
var $author$project$Types$w3_encode_Forest = function (w3v) {
	var v0 = w3v.a;
	return $lamdera$codecs$Lamdera$Wire3$encodeSequenceWithoutLength(
		_List_fromArray(
			[
				$elm$bytes$Bytes$Encode$unsignedInt8(0),
				function (w3_rec_var0) {
				return $lamdera$codecs$Lamdera$Wire3$encodeSequenceWithoutLength(
					_List_fromArray(
						[
							$lamdera$codecs$Lamdera$Wire3$encodeList$($author$project$Types$w3_encode_Forest, w3_rec_var0.kids),
							$lamdera$codecs$Lamdera$Wire3$encodeString(w3_rec_var0.nm),
							$lamdera$codecs$Lamdera$Wire3$encodeInt(w3_rec_var0.z)
						]));
			}(v0)
			]));
var $author$project$Types$w3_encode_FrontendModel = function (w3_rec_var0) {
	return $lamdera$codecs$Lamdera$Wire3$encodeSequenceWithoutLength(
		_List_fromArray(
			[
				$author$project$Types$w3_encode_Ctors(w3_rec_var0.a),
				$lamdera$codecs$Lamdera$Wire3$encodeInt(w3_rec_var0.key),
				$author$project$Types$w3_encode_Nested(w3_rec_var0.nested),
				$author$project$Types$w3_encode_Rec(w3_rec_var0.zeta)
			]));
var $author$project$Types$w3_encode_FrontendMsg = function (w3v) {
	switch (w3v.$) {
		case 'F':
			var v0 = w3v.a;
			return $lamdera$codecs$Lamdera$Wire3$encodeSequenceWithoutLength(
				_List_fromArray(
					[
						$elm$bytes$Bytes$Encode$unsignedInt8(0),
						$author$project$Types$w3_encode_Choice$($author$project$Types$w3_encode_Ctors, $author$project$Types$w3_encode_Nested, v0)
					]));
		case 'FNoop':
			return $elm$bytes$Bytes$Encode$unsignedInt8(1);
		case 'FaMsg':
			return $elm$bytes$Bytes$Encode$unsignedInt8(2);
		default:
			var v0 = w3v.a;
			return $lamdera$codecs$Lamdera$Wire3$encodeSequenceWithoutLength(
				_List_fromArray(
					[
						$elm$bytes$Bytes$Encode$unsignedInt8(3),
						$author$project$Types$w3_encode_Rec(v0)
					]));
	}
var $author$project$Types$w3_encode_Nested = function (w3_rec_var0) {
	return $lamdera$codecs$Lamdera$Wire3$encodeSequenceWithoutLength(
		_List_fromArray(
			[
				$lamdera$codecs$Lamdera$Wire3$encodeList$(
				function (w3_rec_var1) {
					return $lamdera$codecs$Lamdera$Wire3$encodeSequenceWithoutLength(
						_List_fromArray(
							[
								$lamdera$codecs$Lamdera$Wire3$encodeString(w3_rec_var1.pp),
								$lamdera$codecs$Lamdera$Wire3$encodeInt(w3_rec_var1.q),
								$lamdera$codecs$Lamdera$Wire3$encodeBool(w3_rec_var1.r)
							]));
				},
				w3_rec_var0.list),
				function (w3_rec_var1) {
				return $lamdera$codecs$Lamdera$Wire3$encodeSequenceWithoutLength(
					_List_fromArray(
						[
							function (w3_rec_var2) {
							return $lamdera$codecs$Lamdera$Wire3$encodeSequenceWithoutLength(
								_List_fromArray(
									[
										$lamdera$codecs$Lamdera$Wire3$encodeInt(w3_rec_var2.x),
										$lamdera$codecs$Lamdera$Wire3$encodeFloat(w3_rec_var2.xLonger),
										$lamdera$codecs$Lamdera$Wire3$encodeString(w3_rec_var2.yy)
									]));
						}(w3_rec_var1.aInner),
							$lamdera$codecs$Lamdera$Wire3$encodeInt(w3_rec_var1.zInner)
						]));
			}(w3_rec_var0.outer),
				$author$project$Types$w3_encode_Rec(w3_rec_var0.rec)
			]));
var $author$project$Types$w3_encode_Rec = function (w3_rec_var0) {
	return $lamdera$codecs$Lamdera$Wire3$encodeSequenceWithoutLength(
		_List_fromArray(
			[
				$lamdera$codecs$Lamdera$Wire3$encodeString(w3_rec_var0.a),
				$lamdera$codecs$Lamdera$Wire3$encodeUnit(w3_rec_var0.aaa),
				$lamdera$codecs$Lamdera$Wire3$encodeList$($lamdera$codecs$Lamdera$Wire3$encodeInt, w3_rec_var0.ab),
				$lamdera$codecs$Lamdera$Wire3$encodeString(w3_rec_var0.b1),
				$lamdera$codecs$Lamdera$Wire3$encodeChar(w3_rec_var0.b2),
				$lamdera$codecs$Lamdera$Wire3$encodeOrder(w3_rec_var0.bA),
				$lamdera$codecs$Lamdera$Wire3$encodeInt(w3_rec_var0.b_),
				$lamdera$codecs$Lamdera$Wire3$encodeBool(w3_rec_var0.longerName),
				$lamdera$codecs$Lamdera$Wire3$encodeFloat(w3_rec_var0.mid),
				$lamdera$codecs$Lamdera$Wire3$encodeInt(w3_rec_var0.zeta),
				$lamdera$codecs$Lamdera$Wire3$encodeMaybe$($lamdera$codecs$Lamdera$Wire3$encodeInt, w3_rec_var0.zz)
			]));
var $author$project$Types$w3_encode_ToBackend = function (w3v) {
	switch (w3v.$) {
		case 'Tb':
			return $elm$bytes$Bytes$Encode$unsignedInt8(0);
		case 'Tb2':
			var v0 = w3v.a;
			return $lamdera$codecs$Lamdera$Wire3$encodeSequenceWithoutLength(
				_List_fromArray(
					[
						$elm$bytes$Bytes$Encode$unsignedInt8(1),
						$author$project$Types$w3_encode_UsesExt(v0)
					]));
		case 'ToBackendA':
			var v0 = w3v.a;
			return $lamdera$codecs$Lamdera$Wire3$encodeSequenceWithoutLength(
				_List_fromArray(
					[
						$elm$bytes$Bytes$Encode$unsignedInt8(2),
						$author$project$Types$w3_encode_Choice$($lamdera$codecs$Lamdera$Wire3$encodeString, $lamdera$codecs$Lamdera$Wire3$encodeInt, v0)
					]));
		default:
			var v0 = w3v.a;
			return $lamdera$codecs$Lamdera$Wire3$encodeSequenceWithoutLength(
				_List_fromArray(
					[
						$elm$bytes$Bytes$Encode$unsignedInt8(3),
						$author$project$Types$w3_encode_Ctors(v0)
					]));
	}
var $author$project$Types$w3_encode_ToFrontend = function (w3v) {
	switch (w3v.$) {
		case 'T':
			var v0 = w3v.a;
			return $lamdera$codecs$Lamdera$Wire3$encodeSequenceWithoutLength(
				_List_fromArray(
					[
						$elm$bytes$Bytes$Encode$unsignedInt8(0),
						$author$project$Types$w3_encode_Containers(v0)
					]));
		case 'Tf':
			return $elm$bytes$Bytes$Encode$unsignedInt8(1);
		default:
			var v0 = w3v.a;
			return $lamdera$codecs$Lamdera$Wire3$encodeSequenceWithoutLength(
				_List_fromArray(
					[
						$elm$bytes$Bytes$Encode$unsignedInt8(2),
						$author$project$Types$w3_encode_Nested(v0)
					]));
	}
var $author$project$Types$w3_encode_Tree = F2($author$project$Types$w3_encode_Tree$);
var $author$project$Types$w3_encode_Tree$ = function (w3_x_c_a, w3v) {
	if (w3v.$ === 'Leaf') {
		return $elm$bytes$Bytes$Encode$unsignedInt8(0);
	} else {
		var v0 = w3v.a;
		var v1 = w3v.b;
		var v2 = w3v.c;
		return $lamdera$codecs$Lamdera$Wire3$encodeSequenceWithoutLength(
			_List_fromArray(
				[
					$elm$bytes$Bytes$Encode$unsignedInt8(1),
					$author$project$Types$w3_encode_Tree$(w3_x_c_a, v0),
					w3_x_c_a(v1),
					$author$project$Types$w3_encode_Tree$(w3_x_c_a, v2)
				]));
	}
var $author$project$Types$w3_encode_UsesExt = function (w3_rec_var0) {
	return $lamdera$codecs$Lamdera$Wire3$encodeSequenceWithoutLength(
		_List_fromArray(
			[
				function (w3_rec_var1) {
				return $lamdera$codecs$Lamdera$Wire3$encodeSequenceWithoutLength(
					_List_fromArray(
						[
							$lamdera$codecs$Lamdera$Wire3$encodeInt(w3_rec_var1.k),
							$lamdera$codecs$Lamdera$Wire3$encodeBool(w3_rec_var1.kk),
							$lamdera$codecs$Lamdera$Wire3$encodeMaybe$($lamdera$codecs$Lamdera$Wire3$encodeString, w3_rec_var1.m),
							$lamdera$codecs$Lamdera$Wire3$encodeList$($lamdera$codecs$Lamdera$Wire3$encodeInt, w3_rec_var1.zz)
						]));
			}(w3_rec_var0.e),
				$author$project$Other$w3_encode_Person(w3_rec_var0.p),
				function (w3_rec_var1) {
				return $lamdera$codecs$Lamdera$Wire3$encodeSequenceWithoutLength(
					_List_fromArray(
						[
							$lamdera$codecs$Lamdera$Wire3$encodeFloat(w3_rec_var1.a),
							$lamdera$codecs$Lamdera$Wire3$encodeInt(w3_rec_var1.ex),
							$lamdera$codecs$Lamdera$Wire3$encodeString(w3_rec_var1.extra),
							$lamdera$codecs$Lamdera$Wire3$encodeString(w3_rec_var1.name),
							$lamdera$codecs$Lamdera$Wire3$encodeInt(w3_rec_var1.zid)
						]));
			}(w3_rec_var0.q)
			]));
var $author$project$Types$w3_encode_Wrapped = function (w3_x_c_a) {
	return function (w3_rec_var0) {
		return $lamdera$codecs$Lamdera$Wire3$encodeSequenceWithoutLength(
			_List_fromArray(
				[
					$lamdera$codecs$Lamdera$Wire3$encodeList$(w3_x_c_a, w3_rec_var0.at),
					w3_x_c_a(w3_rec_var0.value),
					$lamdera$codecs$Lamdera$Wire3$encodeInt(w3_rec_var0.zcount)
				]));
	};
