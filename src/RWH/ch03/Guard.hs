fromMaybe defval wrapped =
    case wrapped of
        Nothing -> defval
        Just value -> value


x = fromMaybe someFun Nothing
        where someFun = 42