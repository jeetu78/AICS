CREATE DATABASE IF NOT EXISTS AICS;
USE AICS;
CREATE REFERENCE TABLE IF NOT EXISTS AIRPORT
(
	UUID               VARCHAR(32) NOT NULL,
	AIRPORT_CODE       VARCHAR(4) NOT NULL,
	CITY_CODE          VARCHAR(8) NOT NULL,
	CITY_NAME          VARCHAR(32) NOT NULL,
	GEO_CODE           VARCHAR(8) NOT NULL,
	MODIFIED_TIME      TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
	PRIMARY KEY (UUID)
);
CREATE REFERENCE TABLE IF NOT EXISTS FLIGHT
(
	UUID                   VARCHAR(32) NOT NULL,
	FLIGHT_NUMBER          VARCHAR(8) NOT NULL,
	FLIGHT_DATE            DATE NOT NULL,
	CARRIER_CODE           VARCHAR(3) NOT NULL,
	DEPARTURE_AIRPORT_CODE VARCHAR(4) NOT NULL,
	ARRIVAL_AIRPORT_CODE   VARCHAR(4) NOT NULL,
	CITY_PAIR              VARCHAR(16) NOT NULL,
	AIRCRAFT_TYPE_CODE     VARCHAR(20) NOT NULL,
	FLIGHT_STATUS          CHAR(18) NULL,
	MODIFIED_TIME          TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
	PRIMARY KEY (UUID)
);
CREATE REFERENCE TABLE IF NOT EXISTS ANCILLARY_MASTER
(
	UUID                   	VARCHAR(32) NOT NULL,
	ANC_MASTER_CODE        	BIGINT NOT NULL,
	SERVICE_PROVIDER_ID    	CHAR(18) NULL,
	SUB_CODE               	VARCHAR(50) NULL,
	GROUP_CODE             	VARCHAR(50) NULL,
	SUB_GROUP              	VARCHAR(50) NULL,
	DESCRIPTION1           	VARCHAR(100) NULL,
	DESCRIPTION2           	VARCHAR(100) NULL,
	IMAGE_THUMBNAIL_URL     VARCHAR(100) NULL,
	IMAGE_LARGE_URL        	VARCHAR(100) NULL,
	IMAGE_TOOL_TIP         	VARCHAR(100) NULL,
	PRICE                  	FLOAT(9,2) NULL,
	CURRENCY               	CHAR(3) NULL,
	TAX                    	FLOAT(9,2) NULL,
	IS_DISCOUNT            	CHAR(3) NULL,
	DISCOUNT_DESC          	VARCHAR(100) NULL,
	DISCOUNT_PCNT          	FLOAT(2,1) NULL,
	COMMERCIAL_NAME        	VARCHAR(50) NULL,
	RFIC                   	VARCHAR(10) NULL,
	MODIFIED_TIME          	TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
	PRIMARY KEY (UUID)
);
CREATE REFERENCE TABLE IF NOT EXISTS ANCILLARY_INVENTORY
(
	UUID                   	VARCHAR(32) NOT NULL,
	ANC_INVENTORY_ID       	BIGINT NOT NULL,
	ANCILLARY_MASTER_UUID	VARCHAR(32) NOT NULL,
	FLIGHT_UUID            	VARCHAR(32) NOT NULL,
	ALLOCATED_QUANTITY     	INTEGER NULL,
	AVAILABLE_QUANTITY     	INTEGER NULL,
	MODIFIED_TIME          	TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
	PRIMARY KEY (UUID)
);

CREATE REFERENCE TABLE IF NOT EXISTS CUSTOMER
(
	UUID               VARCHAR(32) NOT NULL,
	FFP_NUMBER         VARCHAR(16) NOT NULL,
	FIRST_NAME         VARCHAR(128) NULL,
	LAST_NAME          VARCHAR(128) NULL,
	ADDRESS            VARCHAR(512) NULL,
	MODIFIED_TIME      TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
	PRIMARY KEY (UUID)
);

CREATE REFERENCE TABLE IF NOT EXISTS ANCILLARY_TX
(
	UUID                   		VARCHAR(32) NOT NULL,
	ANC_TXN_ID             		BIGINT NOT NULL,
	ANCILLARY_INVENTORY_UUID	VARCHAR(32) NOT NULL,
	CUSTOMER_UUID          		VARCHAR(32) NOT NULL,
	OPERATION_TYPE         		VARCHAR(50) NULL,
	BOOKING_TIME           		TIMESTAMP NULL,
	QUANTITY               		INTEGER NOT NULL,
	MODIFIED_TIME          		TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
	PRIMARY KEY (UUID)
);


CREATE REFERENCE TABLE IF NOT EXISTS BOOKING_REFERENCE
(
	UUID                   VARCHAR(32) NOT NULL,
	REFERENCE_NUMBER       VARCHAR(20) NOT NULL,
	CUSTOMER_UUID          VARCHAR(32) NOT NULL,
	FLIGHT_UUID            VARCHAR(32) NOT NULL,
	TICKET_NUMBER          VARCHAR(50) NULL,
	TICKET_TYPE            VARCHAR(20) NULL,
	MODIFIED_TIME          TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
	PRIMARY KEY (UUID)
);