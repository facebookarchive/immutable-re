/**
 * Copyright 2004-present Facebook. All Rights Reserved.
 */

module Map = {
  type t 'value;
  external delete: int => t 'value = "" [@@bs.send.pipe : t 'value];
  external fromJS : array 'value => t 'value = "Map" [@@bs.module "immutable"];
  external get :  int => 'value  = "" [@@bs.send.pipe : t 'value];
  external set: int => 'value => t 'value = "" [@@bs.send.pipe : t 'value];
};

module Set = {
  type t 'value;
  external includes : t 'value => 'value => bool = "" [@@bs.send];
  external contains : t 'value => 'value => bool = "" [@@bs.send];
  external fromArray : array 'value => t 'value = "Set" [@@bs.module "immutable"];
};

module List = {
  type t 'value;

  external fromArray : array 'value => t 'value = "List" [@@bs.module "immutable"];
  external get : int => 'value = "" [@@bs.send.pipe : t 'value];
  external pop : t 'value = "" [@@bs.send.pipe : t 'value];
  external push : 'value => t 'value = "" [@@bs.send.pipe : t 'value];
  external set: int => 'value => t 'value = "" [@@bs.send.pipe : t 'value];
};
