-- Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

create table extra_app_activity_record_store
(
    store_id                    int not null,
    record_time                 bigint not null,
    -- The mining round number to which this activity is assigned
    round_number                bigint not null,
    -- App providers for which app activity should be recorded
    app_provider_parties        text[] not null,
    -- Activity weights of the app providers, in one-to-one correspondence with app_provider_parties.
    app_activity_weights        bigint[] not null,
    -- Asset providers for which activity should be recorded
    asset_provider_parties      text[] not null,
    -- Activity weights of the asset providers, in one-to-one correspondence with asset_provider_parties.
    asset_activity_weights      bigint[] not null
);

-- Used to compute the per-party totals for a round
create index extra_app_activity_record_store_sid_rn_idx on
  extra_app_activity_record_store (store_id, round_number);

create index extra_app_activity_record_store_sid_rt_idx on
  extra_app_activity_record_store (store_id, record_time);

-- Metadata for extra activity record ingestion runs, analogous to app_activity_record_meta.
-- The store_id should never be modified as that could result in loss of activity data.
create table extra_app_activity_record_meta
(
    store_id                          int not null,
    -- Record time (microseconds since epoch) at which ingestion started.
    -- Rounds before this time may be partial.
    started_ingesting_at              bigint not null,
    -- The earliest round number in the first batch ingested
    earliest_ingested_round           bigint not null,
    -- The highest OpenMiningRound round number which has been archived
    -- as of the max record_time of the ingested data.
    last_archived_round               bigint,
    last_updated_at                   timestamptz default now(),

    constraint extra_app_activity_record_meta_pkey primary key (store_id)
);

-- Extra app / asset activity totals per party and round.
-- Existing rows have no extra activity, hence the default of 0.
alter table app_activity_party_totals
    add column total_extra_app_activity_weight   bigint not null default 0,
    add column total_extra_asset_activity_weight bigint not null default 0,
    add column num_extra_app_activity_records    bigint not null default 0,
    add column num_extra_asset_activity_records  bigint not null default 0;

alter table app_activity_round_totals
    add column total_round_extra_app_activity_weight   bigint not null default 0,
    add column total_round_extra_asset_activity_weight bigint not null default 0;
