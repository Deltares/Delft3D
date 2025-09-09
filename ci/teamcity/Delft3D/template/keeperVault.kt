package Delft3D.template

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*

object TemplateKeeperVault : Template({

    name = "Keeper Vault"
    description = "Make Keeper Vault secrets available"

    features {
        provideKeeperCredentials {
            connectionId = "KEEPER_VAULT_DELFT3D"
        }
    }
})
