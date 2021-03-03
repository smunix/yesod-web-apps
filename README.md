# yesod-web-apps
Selected Web apps

## Ghcid for RDD (Rapid Development Driven)
This is a way to setup a quick feedback loop (The traditional Yesod devel solution is painfully slow).
The example below shows how to work with Amahoro, which was created with the Scaffolding Yesod tool:

```sh
nix shell ../#apps/amahoro -c ghcid -W --test='DevelMain.update' -c 'APP_ENV=development stack ghci --no-build --no-load --work-dir .stack-work-devel amahoro' 
```

The above will compile Amahoro Web app in a pristine pure nix-shell environment first, running Haddock generation and the tests as well. In a rapid development
environment, all of this takes quite some time. In a attempt to shorten the build procedure, you can safely get rid of the nix shell invokation above and 
directly run:

``` sh
ghcid -W --test='DevelMain.update' -c 'APP_ENV=development stack ghci --no-build --work-dir .stack-work-devel amahoro'
```

It works wonders!

## festhest ghcid
Start with
``` sh
ghcid -W --test='main' -c 'cabal repl' -a
```

## Start a Qemu/kvm Server with amahoro and festhest both running
```sh
QEMU_NET_OPTS=hostfwd=tcp::3080-:3080,hostfwd=tcp::3000-:3000 nix shell github:smunix/yesod-web-apps#qemu/olhajwon -c run-olhajwon-vm
```
