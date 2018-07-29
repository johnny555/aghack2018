module Api where

import           Data.Aeson hiding (defaultOptions)
import           Data.Text
import           GHC.Generics
import           Servant.API
import Data.ByteString

{-type RoleAPI =
  "roles" :>  Get '[JSON] [Role]

  :<|> "roleConnections" :> QueryParam "project" Text :> Get '[JSON] [(Role, Role)]

  :<|> "add" :> QueryParam "project" Text :>  ReqBody '[JSON] Text :> Post '[JSON] Bool
-}

