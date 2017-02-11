type ownerT = | Owner;
type owner = ref ownerT;

type transient 'a = {
  owner: owner,
  mutable editable: bool,
  mutable value: 'a,
};

let create (value: 'a): (transient 'a) => ({
  owner: ref  Owner,
  editable: true,
  value: value,
});

let ensureEditable ({ editable } as transient: transient 'a): (transient 'a) => (not editable)
  ? { failwith "Transient has already been persisted" }
  : transient;

let get (transient: transient 'a): 'a => {
  let { value } = ensureEditable transient;
  value
};

let persist (transient: transient 'a): 'a =>  {
  let transient = ensureEditable transient;
  transient.editable = false;
  transient.value
};

let update (f: owner => 'a => 'a) (transient: transient 'a): (transient 'a) => {
  let transient = ensureEditable transient;
  transient.value = f transient.owner transient.value;
  transient
};
