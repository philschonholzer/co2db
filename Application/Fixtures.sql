

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;


SET SESSION AUTHORIZATION DEFAULT;

ALTER TABLE public.categories DISABLE TRIGGER ALL;

INSERT INTO public.categories (id, title) VALUES ('64ac4651-062b-47db-ae5f-9929dc73950d', 'Agriculture');
INSERT INTO public.categories (id, title) VALUES ('d20e8aa8-384e-405a-90a2-bf30554bf9bb', 'Transportation');
INSERT INTO public.categories (id, title) VALUES ('b0871cb0-3e77-4b90-812e-006106af8a84', 'Electricity');
INSERT INTO public.categories (id, title) VALUES ('092a5cd4-1f4f-4db4-b79c-918dc7fdca2d', 'Buildings');
INSERT INTO public.categories (id, title) VALUES ('f26219b3-a8b9-4f94-93aa-161248f304d0', 'Industrie');


ALTER TABLE public.categories ENABLE TRIGGER ALL;


ALTER TABLE public.users DISABLE TRIGGER ALL;

INSERT INTO public.users (id, email, password_hash, locked_at, failed_login_attempts) VALUES ('8bd2a484-21d6-4b58-b4e4-fa564a3f0e9c', 'test@t.ch', 'sha256|17|B5oqx44joFpibTqos43sxg==|7f0eiZBIxGQo+WopsZL1Up3gV9D14b0tV/pAr3ttjIY=', NULL, 0);
INSERT INTO public.users (id, email, password_hash, locked_at, failed_login_attempts) VALUES ('7f12d3d6-8aca-4bd6-822c-c21e94cd2fc8', 'phi.sch@hotmail.ch', 'sha256|17|Eo4Sh9ZAk9jIW3znFCsMGA==|arfAgk/cnbYl0xn+j3j9E0m/2QxwfRBZGJFo73kQStM=', NULL, 0);


ALTER TABLE public.users ENABLE TRIGGER ALL;


ALTER TABLE public.co2_producers DISABLE TRIGGER ALL;

INSERT INTO public.co2_producers (id, title, description, category_id, image, user_id, unit, common_single_consumption_from, common_single_consumption_to, common_single_consumption_average, common_yearly_consumption_from, common_yearly_consumption_to, common_yearly_consumption_average) VALUES ('c8d6f416-ad52-484f-9f7e-b510dac2c158', 'Beef Meat', 'CO2 emissions for the production of beef meat.', '64ac4651-062b-47db-ae5f-9929dc73950d', NULL, '7f12d3d6-8aca-4bd6-822c-c21e94cd2fc8', 'kilogram', 0, 1, 0.15, 0, 100, 26.35);
INSERT INTO public.co2_producers (id, title, description, category_id, image, user_id, unit, common_single_consumption_from, common_single_consumption_to, common_single_consumption_average, common_yearly_consumption_from, common_yearly_consumption_to, common_yearly_consumption_average) VALUES ('71eb436f-ee25-4fb3-8c87-535bee7906e6', 'Per Capita (per person)', NULL, '64ac4651-062b-47db-ae5f-9929dc73950d', NULL, '7f12d3d6-8aca-4bd6-822c-c21e94cd2fc8', 'hour', 24, 24, 24, 8760, 8760, 8760);
INSERT INTO public.co2_producers (id, title, description, category_id, image, user_id, unit, common_single_consumption_from, common_single_consumption_to, common_single_consumption_average, common_yearly_consumption_from, common_yearly_consumption_to, common_yearly_consumption_average) VALUES ('67ac058e-4156-404e-8933-fa43e950550c', 'Shower', NULL, '092a5cd4-1f4f-4db4-b79c-918dc7fdca2d', NULL, '7f12d3d6-8aca-4bd6-822c-c21e94cd2fc8', 'minute', 0, 60, 13, 0, 20000, 4745);
INSERT INTO public.co2_producers (id, title, description, category_id, image, user_id, unit, common_single_consumption_from, common_single_consumption_to, common_single_consumption_average, common_yearly_consumption_from, common_yearly_consumption_to, common_yearly_consumption_average) VALUES ('05e9b91f-fe9e-4679-86c7-fe60fa13fae4', 'Gasoline', NULL, 'd20e8aa8-384e-405a-90a2-bf30554bf9bb', NULL, '7f12d3d6-8aca-4bd6-822c-c21e94cd2fc8', 'liter', 0, 100, 40, 0, 10000, 2483);
INSERT INTO public.co2_producers (id, title, description, category_id, image, user_id, unit, common_single_consumption_from, common_single_consumption_to, common_single_consumption_average, common_yearly_consumption_from, common_yearly_consumption_to, common_yearly_consumption_average) VALUES ('4067b5fe-582d-4cc1-948b-3c704aebd5fa', 'LED Lightbulb', '7W', 'b0871cb0-3e77-4b90-812e-006106af8a84', NULL, '7f12d3d6-8aca-4bd6-822c-c21e94cd2fc8', 'hour', 0, 24, 8, 0, 5000, 1780);


ALTER TABLE public.co2_producers ENABLE TRIGGER ALL;



ALTER TABLE public.sources DISABLE TRIGGER ALL;

INSERT INTO public.sources (id, co2_producer_id, region, year, g_co2e, per, description, user_id) VALUES ('1eb19744-e370-4cde-a4ba-a864b407f819', '4067b5fe-582d-4cc1-948b-3c704aebd5fa', NULL, NULL, 3, 1, '<https://www.eia.gov/tools/faqs/faq.php?id=74&t=11#:~:text=This%20equaled%20about%200.92%20pounds%20of%20CO2%20emissions%20per%20kWh>', '7f12d3d6-8aca-4bd6-822c-c21e94cd2fc8');
INSERT INTO public.sources (id, co2_producer_id, region, year, g_co2e, per, description, user_id) VALUES ('2eb19744-e370-4cde-a4ba-a864b407f818', 'c8d6f416-ad52-484f-9f7e-b510dac2c158', NULL, NULL, 67800, 1, '<http://www.fao.org/3/i3461e/i3461e03.pdf>', '7f12d3d6-8aca-4bd6-822c-c21e94cd2fc8');
INSERT INTO public.sources (id, co2_producer_id, region, year, g_co2e, per, description, user_id) VALUES ('3eb19744-e370-4cde-a4ba-a864b407f817', '71eb436f-ee25-4fb3-8c87-535bee7906e6', NULL, NULL, 685, 1, '<https://ourworldindata.org/grapher/consumption-co2-per-capita?year=latest>', '7f12d3d6-8aca-4bd6-822c-c21e94cd2fc8');
INSERT INTO public.sources (id, co2_producer_id, region, year, g_co2e, per, description, user_id) VALUES ('4eb19744-e370-4cde-a4ba-a864b407f816', '67ac058e-4156-404e-8933-fa43e950550c', NULL, NULL, 2000, 10, '<https://theecoguide.org/have-you-tried-five-minute-shower-challenge>', '7f12d3d6-8aca-4bd6-822c-c21e94cd2fc8');
INSERT INTO public.sources (id, co2_producer_id, region, year, g_co2e, per, description, user_id) VALUES ('7eb19744-e370-4cde-a4ba-a864b407f815', '05e9b91f-fe9e-4679-86c7-fe60fa13fae4', NULL, NULL, 2312, 1, '<https://www.nrcan.gc.ca/sites/www.nrcan.gc.ca/files/oee/pdf/transportation/fuel-efficient-technologies/autosmart_factsheet_6_e.pdf>', '7f12d3d6-8aca-4bd6-822c-c21e94cd2fc8');
INSERT INTO public.sources (id, co2_producer_id, region, year, g_co2e, per, description, user_id) VALUES ('f2b42e2b-875c-4dd1-8c4b-603a0be34de8', '4067b5fe-582d-4cc1-948b-3c704aebd5fa', 'Switzerland', 2018, 1, 1, '<https://www.bafu.admin.ch/bafu/de/home/themen/klima/fragen-antworten.html#:~:text=Die%20Kennzahlen%20f%C3%BCr%20die%20verschiedenen,Strommix%3A%2015.7%20g%20CO2eq%2FkWh>', '7f12d3d6-8aca-4bd6-822c-c21e94cd2fc8');


ALTER TABLE public.sources ENABLE TRIGGER ALL;


