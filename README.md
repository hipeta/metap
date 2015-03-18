# Metap

Metap provides metaclass propagation along class inheritance structure.
Metap uses closer-mop and changes *c2mop:ensure-class name &rest args* in *metap:with-metap-ensured* macro.

## Motivation

You got some idea which use metaclass like

```
(defclass some-meta-class (standard-class) ())
(defmethod something1 ((class some-meta-class) ...) ...)
(defmethod something2 ((class some-meta-class) ...) ...)
```

and use this for some classes like

```
(defclass some-class1 () () (:metaclass 'some-meta-class))
(defclass some-class2 (some-class1) () (:metaclass 'some-meta-class))
(defclass some-class3 (some-class2) () (:metaclass 'some-meta-class))
(defclass some-class4 (some-class1) () (:metaclass 'some-meta-class))
... :metaclass :metaclass :metaclass :metaclass
```

This is boring.
Using metap, it can simply be written like

```
(defclass some-mixin () ())
(metap:register-m1-m2-pair 'some-mixin 'some-meta-class)

(metap:enable-metap)

(defclass some-class1 (some-mixin) ())
(defclass some-class2 (some-class1) ())
(defclass some-class3 (some-class2) ())
(defclass some-class4 (some-class1) ())
```

or you want metap to be local, 

```
(defclass some-mixin () ())
(metap:register-m1-m2-pair 'some-mixin 'some-meta-class)

(metap:with-metap
  (defclass some-class1 (some-mixin) ())
  (defclass some-class2 (some-class1) ())
  (defclass some-class3 (some-class2) ())
  (defclass some-class4 (some-class1) ()))
```

Also see cl-singleton-mixin (https://github.com/hipeta/cl-singleton-mixin) which is written by using metap.

## Installation

1. Download metap and move the directory to quicklisp local-project directory.
1. (ql:quickload :metap)

## License

Metap is released under the MIT License, see LICENSE file.
