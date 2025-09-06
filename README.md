# Heartbeat

A robust Erlang/OTP service for tracking user online/offline status with persistent history storage using Mnesia database.

## Overview

Heartbeat is a high-performance service designed to monitor and track user presence in real-time systems. It provides a simple HTTP API for clients to maintain their online status and retrieve user presence information along with historical data.

## Features

- **Real-time Status Tracking**: Track user online/offline status with automatic expiration
- **Persistent Storage**: Uses Mnesia database for reliable data persistence
- **HTTP REST API**: Simple REST endpoints for easy integration
- **Historical Data**: Complete timeline of user online/offline sessions
- **Fault Tolerance**: Built on Erlang/OTP supervision principles
- **High Performance**: Designed to handle concurrent users efficiently

## Architecture

The service is built using:
- **Erlang/OTP**: For concurrent, fault-tolerant processing
- **Mnesia**: Distributed database for data persistence
- **Cowboy**: HTTP server for REST API
- **JSX**: JSON encoding/decoding
- **Lager**: Structured logging

## Installation

### Prerequisites

- Erlang/OTP 21+ 
- Rebar3

### Dependencies

The application requires the following Erlang libraries:
- `mnesia` - Database
- `cowboy` - HTTP server
- `ranch` - Socket acceptor pool
- `jsx` - JSON library
- `lager` - Logging framework

### Building

```bash
rebar3 compile
```

### Running

```bash
rebar3 shell
```

The service will start on port `3519` by default.

## API Reference

### PUT /heal/:user_id

Marks a user as online and extends their online status.

**Parameters:**
- `user_id` (integer): Unique user identifier

**Response:**
```json
{
  "status": "online"
}
```

**Usage:**
This endpoint should be called every 7 seconds from the client to maintain online status. Each call extends the user's online status by 7 seconds from the current time.

**Example:**
```bash
curl -X PUT http://localhost:3519/heal/12345
```

### GET /checkup/:user_id

Retrieves the current online/offline status of a user.

**Parameters:**
- `user_id` (integer): Unique user identifier

**Response:**
```json
{
  "id": 12345,
  "status": "online"
}
```

**Status Values:**
- `online`: User is currently active
- `offline`: User session has expired

**Example:**
```bash
curl http://localhost:3519/checkup/12345
```

### GET /heart_rhythm/:user_id

Retrieves the complete online/offline history for a user.

**Parameters:**
- `user_id` (integer): Unique user identifier

**Response:**
```json
{
  "id": 12345,
  "history": [
    {
      "online_at": 1638360000,
      "offline_at": 1638360300
    },
    {
      "online_at": 1638361000,
      "offline_at": 1638361450
    }
  ]
}
```

**Note:** If no history exists for the user, returns an empty history array.

**Example:**
```bash
curl http://localhost:3519/heart_rhythm/12345
```

## Database Schema

The service uses Mnesia with the following table structure:

### history table
- `user_id` (integer): User identifier
- `online_at` (integer): Timestamp when user came online (Unix seconds)
- `offline_at` (integer): Timestamp when user goes offline (Unix seconds)

## Configuration

### Port Configuration
The default port (3519) can be modified in `heartbeat_server.erl`:

```erlang
cowboy:start_clear(http_listener, [{port, 3519}], #{env => #{dispatch => Dispatch}})
```

### Session Timeout
The default session timeout (7 seconds) can be adjusted in `heartbeat_store.erl`:

```erlang
Expire = Timestamp + 7,
```

## Error Handling

The API returns appropriate HTTP status codes:

- `200 OK`: Successful operation
- `400 Bad Request`: Invalid user_id format or missing parameters
- `500 Internal Server Error`: Database or server errors

Error responses include descriptive JSON messages:
```json
{
  "error": "invalid user_id format"
}
```

## Monitoring and Benchmarking

The service includes a benchmarking module (`heartbeat_bench.erl`) for performance testing:

```erlang
%% Simulate 100 users for 60 seconds
heartbeat_bench:start(100, 60000).
```

This will:
- Create 100 concurrent user sessions
- Each user calls `/heal` with randomized intervals
- Monitor system resources and database growth

## Logging

The service uses Lager for structured logging. Logs include:
- Request processing errors
- Database transaction failures
- Invalid input validation
- Performance metrics
