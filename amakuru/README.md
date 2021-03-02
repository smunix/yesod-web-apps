## Installation

[Install Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/#installupgrade)

```bash
stack build yesod-bin cabal-install --install-ghc
stack install yesod
stack install persistent-sqlite
```

## Execution

* Execute with nix (flake) `nix shell github:smunix/yesod-web-apps#apps/amakuru -c amakuru`
* Execute the file via command line `./Item.hs` or `stack runghc ./Item.hs`. Note that the first run will
take a while, as Stack will build the project.
* Navigate to http://localhost:3000/


## RESTful

http://localhost:3000/api/v1.0/items?_accept=application/json

All credits go to: git@github.com:Gizra/yesod-form-restful-example.git
