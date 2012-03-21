module VM where
import Control.Monad.Error (throwError)
import Types

exec :: VM -> Op -> ThrowsError SchemeValue
exec _ (Constant k _) = return k
exec _ badValue = throwError $ RuntimeError badValue