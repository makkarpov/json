pray-json [![Build Status](https://travis-ci.org/makkarpov/pray-json.svg?branch=master)](https://travis-ci.org/makkarpov/pray-json) [![Latest version](https://maven-badges.herokuapp.com/maven-central/ru.makkarpov/pray-json_2.11/badge.svg?subject=version)](http://search.maven.org/#search%7Cga%7C1%7Cpray-json%20AND%20g%3A%22ru.makkarpov%22) 
=========

**Fully automatic generation of `play-json` serializers**

```
libraryDependencies += "ru.makkarpov" %% "pray-json" % VERSION
```

---------------------

Features (see [tests](https://github.com/makkarpov/pray-json/blob/master/src/test/scala/ru/makkarpov/extjson/JsonSuite.scala)):

* **fully recursive generation** — no need for intermediate serializer implicits;
   * generation can be restricted using `@requireImplicit` annotation.
   * implicit search for `Format[T]` happens also in a companion object of `T`, and, if companion itself is inside of object — in all outer objects.
   * recursive structures are also supported.
* generation of `case class` serializers:
   * simple case classes;
   * renamed fields using `@key` annotation. it also can transform key names to snake_case automatically;
   * support for default values;
   * support for `Option`al fields.
   * in some APIs fields should be serialized as strings even if they aren't strings. This is also supported;
   * inline formatting of case classes with single parameter
* generation of `sealed` serializers:
   * automatically added `type` field to distinguish between cases;
   * name and value of `type` field can be customized with annotations;
   * support for fallback cases;
* generation of serializers for collections:
   * `Traversable`s: `Seq`s, `Set`s, ...;
   * `Map`s: custom key types are supported via `KeyFormat[K]`;
   * `Option`s (when not inside of a case class) are serialized either as `[]` or `[something]`.
* Enumerations:
   * `Value`s are serialized as strings with their name;
   * `ValueSet`s are serialized as array of strings.