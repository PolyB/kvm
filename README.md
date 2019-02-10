# kvm
Haskell bindings for KVM using MonadTransformers

## Building

To build kvm-exe, you need [stack](haskellstack.org)

```sh
stack build
```

# Execute
```sh
stack exec kvm-exe -- bzImage --initrd initrd.cpio.gz serial=ttyS0
```
