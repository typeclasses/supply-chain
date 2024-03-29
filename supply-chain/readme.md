A *supply chain* represents a flow of information from one `Vendor` to the next,
and so on, ultimately reaching a `Job` that returns a product.

```haskell
run (vendor1 >-> vendor2 >-> vendor3 >- job)
```

A job or vendor can place an `order`, which is fulfilled by the vendor
*upstream* of it. In the above example:

* `vendor2` is downstream of `vendor1`; the orders made by `vendor2` are served
  by `vendor1`.
* The orders made by the `job` are served by `vendor3`.
* The orders made by `vendor3` are served by `vendor2`.
* `vendor1` does not make any requests (its upstream interface is `Const Void`).


## Interfaces

An *interface* is a type constructor (kind `Type -> Type`) that describes the
requests and responses exchanged between a vendor and a job.

If a job's upstream interface is `i`, then when the job makes a request of type
`i x`, it receives a response of type `x`.

Values of a type of this kind represent requests. Each constructor will
typically have a constraint that specifies what type of response is expected in
return. Types of this kind are therefore often GADTs. Types of this kind are
also often not functors.

The lack of any interface at all can be expressed as `Const Void`.


## Actions

An *action* is a monadic context such as `IO`.

The lack of any actions at all can be expressed as `Const Void`. (If you are
used to dealing with monad transformers, you might be familiar with using
`Identity` as a trivial context, but such a layer is not needed here.)


## Jobs

`Job up action product` is a monadic context that supports:

  - Making requests on the `up` interface
  - Performing side effects in the `action` context
  - Returning a single `product` value

```
            ▲   │
      up a  │   │  a
            │   ▼
┌─────────────────────────┐
│  Job up action product  │
└─────────────────────────┘
              │
              │  product
              ▼
```


## Writing jobs

`Job` belongs to the `Monad` class, and there are two functions for making
requests and performing actions respectively:

```haskell
order :: up product -> Job up action product
```

```haskell
perform :: action product -> Job up action product
```

A job may also be altered by connecting it to a vendor; see *Vendor-to-job
connection* below.


## Vendors

`Vendor up down action` can:

  - Respond to requests received via the `down` interface
  - Make requests on the `up` interface
  - Perform side effects in the `action` context

```
              ▲   │
        up a  │   │  a
              │   ▼
┌───────────────────────────┐
│   Vendor up down action   │
└───────────────────────────┘
              ▲   │
      down b  │   │  b
              │   ▼
```

The most common way to use vendors is to connect them to jobs using `(>->)` and
`(>-)`.


## Vendor-to-vendor connection

If `i` is the downstream interface of vendor `v1` and the upstream interface of
vendor `v2`, then we can form the composition `v1 >-> v2`.

```haskell
(>->) :: Vendor up i action
      -> Vendor i down action
      -> Vendor up down action
```

```
             ▲   │
       up a  │   │  a
             │   ▼              ─┐
┌────────────────────────┐       │
│   Vendor up i action   │  v1   │
└────────────────────────┘       │
             ▲   │               │
        i b  │   │  b            │  v1 >-> v2
             │   ▼               │
┌────────────────────────┐       │
│  Vendor i down action  │  v2   │
└────────────────────────┘       │
             ▲   │              ─┘
     down c  │   │  c
             │   ▼
```

When the downstream vendor makes a request of type `i b`, the upstream vendor
replies with a response of type `b`.

`(>->)` and `id` form a category.

```haskell
id :: Vendor i i action  -- from "SupplyChain.Vendor"
```

The `(>->)` operator is associative, and `id` is its identity.

- `(a >-> b) >-> c` = `a >-> (b >-> c)`
- `a >-> id` = `a`
- `a` = `id >-> a`


## Vendor-to-job connection

If `i` is the downstream interface of vendor `v` and the upstream interface of
job `j`, then we can form the composition `v >- j`.

```haskell
(>-) :: Vendor up i action
     -> Job i action product
     -> Job up action product
```

```
             ▲   │
       up a  │   │  a
             │   ▼              ─┐
┌────────────────────────┐       │
│   Vendor up i action   │  v    │
└────────────────────────┘       │
             ▲   │               │
        i b  │   │  b            │  v >- j
             │   ▼               │
┌────────────────────────┐       │
│  Job i action product  │  j    │
└────────────────────────┘       │
              │                 ─┘
              │  product
              ▼
```

When the job makes a request of type `i b`, the vendor replies with a response
of type `b`.

`(>->)` and `(>-)` together are associative.

- `(a >-> b) >- c` = `a >-> (b >- c)`


## Writing vendors

We define vendors using the `Vendor` constructor. Please inspect its type
carefully:

```haskell
forall product. down product -> Job up action (Referral up down action product)
```

A vendor is a function that accepts a request. The request type is polymorphic
but constrained by the vendor's downstream interface.

```haskell
forall product. down product -> Job up action (Referral up down action product)
                ^^^^^^^^^^^^
```

A vendor has an upstream interface and can do everything a job can, therefore
the request handler operates in a `Job` context.

```haskell
forall product. down product -> Job up action (Referral up down action product)
                                ^^^^^^^^^^^^^
```

This allows the vendor to undertake a monadic sequence involving `order` and
`perform` while fulfilling the request.

The final step in fulfilling a request is to return a `Referral`.

```haskell
forall product. down product -> Job up action (Referral up down action product)
                                               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

A `Referral` is written using its `Referral` constructor, which has two parameters:

```haskell
Referral :: product -> Vendor up down action -> Referral up down action product
```

The first is the vendor's response to the client's request.

```haskell
Referral :: product -> Vendor up down action -> Referral up down action product
            ^^^^^^^
```

The second is a new `Vendor`.

```haskell
Referral :: product -> Vendor up down action -> Referral up down action product
                       ^^^^^^^^^^^^^^^^^^^^^
```

This latter component is what allows vendors to be stateful, and it is usually
defined recursively.


## Notes on package versioning

This `supply-chain` package re-exports types and values from
`supply-chain-core`. Since a `supply-chain-core` API change can cause a
`supply-chain` API change, our version bounds on the `supply-chain-core`
dependency must include three digits. A major or minor version bump in
`supply-chain-core` prompts a corresponding bump in `supply-chain` if any
re-exported entities have changed.
