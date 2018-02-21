/***
 * Copyright 2004-present Facebook. All Rights Reserved.
 */
module Map = {
  type t('value);
  [@bs.send.pipe : t('value)] external delete : int => t('value) = "";
  [@bs.module "immutable"] external fromJS : array('value) => t('value) = "Map";
  [@bs.send.pipe : t('value)] external get : int => 'value = "";
  [@bs.send.pipe : t('value)] external set : (int, 'value) => t('value) = "";
};

module Set = {
  type t('value);
  [@bs.send] external includes : (t('value), 'value) => bool = "";
  [@bs.send] external contains : (t('value), 'value) => bool = "";
  [@bs.module "immutable"] external fromArray : array('value) => t('value) = "Set";
};

module List = {
  type t('value);
  [@bs.module "immutable"] external fromArray : array('value) => t('value) = "List";
  [@bs.send.pipe : t('value)] external get : int => 'value = "";
  [@bs.send.pipe : t('value)] external pop : t('value) = "";
  [@bs.send.pipe : t('value)] external push : 'value => t('value) = "";
  [@bs.send.pipe : t('value)] external set : (int, 'value) => t('value) = "";
};
