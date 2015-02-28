# Metap

Metap provides metaclass propagation along class inheritance structures.
Metap use closer-mop and modify c2mop:ensure-class-usgin-class by around method so it can conflict with some libraries modifing same method.

## Motivation

You got some idea which use mop like

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

(defclass some-class1 (some-mixin) ())
(defclass some-class2 (some-class1) ())
(defclass some-class3 (some-class2) ())
(defclass some-class4 (some-class1) ())
```

## Environment

- sbcl 1.2.1 or higher

## Usage

1. Download arrow-macros (https://github.com/hipeta/arrow-macros) and move the directory to quicklisp local-project directory.
1. Download metap and move the directory to quicklisp local-project directory.
1. (ql:quickload :metap)
1. (metap:register-m1-m2-pair your-class your-metaclass) and then subclasses of your-class are instantiated from your-metaclass by defclass. (so subclasses of your-class must be defined after metap:register-m1-m2-pair)

See cl-singleton-mixin (https://github.com/hipeta/cl-singleton-mixin) which is written by using metap.

## License

Metap is released under the MIT License, see LICENSE file.
