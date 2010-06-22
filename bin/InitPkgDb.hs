import Distribution.Dev.InitPkgDb ( initPkgDb )
import Distribution.Dev.LocalRepo ( resolveSandbox )
main = initPkgDb =<< resolveSandbox []