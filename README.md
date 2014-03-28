# PJ Model Bson

Convert pjm model to/from bson.

## Usage

Add pjm_bson as a dependency in `rebar.config`.

    {deps, [{pjm_bson, ".*", {git, "git://github.com/3pjgames/pjm_bson.git", {tag, "1.0.0"}}}]}.

- `pjm_bson:to_bson(Model :: pjm:model()) -> bson:document()`
- `pjm_bson:from_bson(Bson :: bson:document(), Model :: pjm:model()) -> pjm:model()`
