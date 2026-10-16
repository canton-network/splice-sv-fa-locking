alter table dso_acs_store
  add column governance_lock_type text,
  add column governance_lock_subject text;

create index dso_acs_store_sid_mid_glt_gls
    on dso_acs_store (store_id, migration_id, governance_lock_type, governance_lock_subject)
    where governance_lock_type is not null;

drop index dso_acs_store_provisional_featured_app_lock_for;
alter table dso_acs_store
 drop column provisional_featured_app_lock_for;

alter table scan_acs_store
  add column governance_lock_type text,
  add column governance_lock_subject text;

create index scan_acs_store_sid_mid_glt_gls
    on scan_acs_store (store_id, migration_id, governance_lock_type, governance_lock_subject)
    where governance_lock_type is not null;
