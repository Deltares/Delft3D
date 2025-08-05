package Delft3D.template

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*

object TemplateKeeperVault : Template({

    name = "Keeper Vault"
    description = "Connection to Keeper Vault"

    features {
        feature {
            id = "KEEPER_VAULT_DELFT3D"
            type = "JetBrains.KeeperVault"
            param("connectionId", "KEEPER_VAULT_DELFT3D")
        }
    }
})
