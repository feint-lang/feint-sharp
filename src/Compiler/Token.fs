module Feint.Compiler.Token

type Token =
    | Nil
    | True
    | False
    | Int of bigint
    | Star
    | Slash
    | Plus
    | Dash

type PosToken =
    { startPos: (uint * uint)
      endPos: (uint * uint)
      token: Token }
