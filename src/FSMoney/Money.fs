namespace FSMoney

module Money =
    // TODO: Add support for divide.

    type Money = { Amount: int; Currency: string }

    type Cmp =
        | Lt
        | Eq
        | Gt

    let internal failCurrenciesMustEqual a b =
        invalidArg "Money.Currency" (sprintf "Currency %s must be the same as %s" a.Currency b.Currency)

    let private (| Ltt | Eqq | Gtt |) (a,b) =
        match (a.Currency, b.Currency) with
        | (c1, c2) when c1 = c2 ->
            match a.Amount - b.Amount with
            | x when x < 0 -> Ltt
            | x when x = 0 -> Eqq
            | _  -> Gtt
        | _ -> failCurrenciesMustEqual a b

    let create amount currency = { Amount = amount; Currency = currency }

    let isZero { Amount = amount } = amount = 0

    let isPositive { Amount = amount } = amount > 0

    let isNegative { Amount = amount } = amount < 0

    let equals a1 a2 = a1 = a2

    let cmp a b =
        match a, b with
        | Ltt -> Lt
        | Eqq -> Eq
        | _ -> Gt

    let addFloat m v =
        { Amount = m.Amount + ((v * 100.0) |> round |> int)
          Currency = m.Currency }

    let addInt m v =
        { Amount = m.Amount + v
          Currency = m.Currency }

    let add m v =
        match box v with
        | :? float as f -> addFloat m f
        | :? int as n -> addInt m n
        | _ -> invalidArg "v" "v must be int or float"

    let subtractFloat m v =
        { Amount = m.Amount - ((v * 100.0) |> round |> int)
          Currency = m.Currency }

    let subtractInt m v =
        { Amount = m.Amount - v
          Currency = m.Currency }

    let subtract m v =
        match box v with
        | :? float as f -> subtractFloat m f
        | :? int as n -> subtractInt m n
        | _ -> invalidArg "v" "v must be int or float"

    let multiplyInt m v =
        { Amount = m.Amount * v
          Currency = m.Currency }

    let multiplyFloat m v =
        { Amount = (float m.Amount) * v |> round |> int
          Currency = m.Currency }

    let multiply m v =
        match box v with
        | :? float as f -> multiplyFloat m f
        | :? int as n -> multiplyInt m n
        | _ -> invalidArg "v" "v must be int or float"
