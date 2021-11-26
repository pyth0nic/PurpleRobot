[<AutoOpen>]
module Util
  let name x = x.GetType.ToString()
  let fst3 (x,_,_) = x  
  let snd3 (_,x,_) = x  
  let thd3 (_,_,x) = x

  let str x = x.ToString()
  