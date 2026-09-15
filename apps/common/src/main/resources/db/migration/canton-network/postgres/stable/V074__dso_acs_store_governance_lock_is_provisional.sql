alter table dso_acs_store
  add column governance_lock_is_provisional boolean;

create index dso_acs_store_governance_lock_is_provisional
    on dso_acs_store (store_id, migration_id, template_id_qualified_name, governance_lock_is_provisional)
    where governance_lock_is_provisional;
