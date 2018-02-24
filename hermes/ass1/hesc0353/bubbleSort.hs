bubbleSort [] = []
bubbleSort (h:t) = bubbleSelect (h:(bubbleSort t))
  where
    bubbleSelect [] = []
    bubbleSelect [a] = [a]
    bubbleSelect (a:b:t) =
    if a > b then b:(bubbleSelect (a:t))
    else a:(bubbleSelect (b:t))
