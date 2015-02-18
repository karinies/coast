/*  =========================================================================
    zre_msg - work with ZRE messages

    Codec class for zre_msg.

    ** WARNING *************************************************************
    THIS SOURCE FILE IS 100% GENERATED. If you edit this file, you will lose
    your changes at the next build cycle. This is great for temporary printf
    statements. DO NOT MAKE ANY CHANGES YOU WISH TO KEEP. The correct places
    for commits are:

    * The XML model used for this code generation: zre_msg.xml
    * The code generation script that built this file: zproto_codec_c
    ************************************************************************
    
    Copyright (c) 1991-2012 iMatix Corporation -- http://www.imatix.com     
    Copyright other contributors as noted in the AUTHORS file.              
                                                                            
    This file is part of Zyre, an open-source framework for proximity-based 
    peer-to-peer applications -- See http://zyre.org.                       
                                                                            
    This is free software; you can redistribute it and/or modify it under   
    the terms of the GNU Lesser General Public License as published by the  
    Free Software Foundation; either version 3 of the License, or (at your  
    option) any later version.                                              
                                                                            
    This software is distributed in the hope that it will be useful, but    
    WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTA-   
    BILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General  
    Public License for more details.                                        
                                                                            
    You should have received a copy of the GNU Lesser General Public License
    along with this program. If not, see http://www.gnu.org/licenses/.      
    =========================================================================
*/

/*
@header
    zre_msg - work with ZRE messages
@discuss
@end
*/

#include <czmq.h>
#include "../include/zre_msg.h"

//  Structure of our class

struct _zre_msg_t {
    zframe_t *routing_id;               //  Routing_id from ROUTER, if any
    int id;                             //  zre_msg message ID
    byte *needle;                       //  Read/write pointer for serialization
    byte *ceiling;                      //  Valid upper limit for read pointer
    uint16_t sequence;                  //  Incremental sequence number
    char *ipaddress;                    //  Sender IP address
    uint16_t mailbox;                   //  Sender mailbox port numer
    zlist_t *groups;                    //  List of groups sender is in
    byte status;                        //  Sender groups status sequence
    zhash_t *headers;                   //  Sender header properties
    size_t headers_bytes;               //  Size of dictionary content
    zmsg_t *content;                    //  Wrapped message content
    char *group;                        //  Group to send to
};

//  --------------------------------------------------------------------------
//  Network data encoding macros

//  Put a block of octets to the frame
#define PUT_OCTETS(host,size) { \
    memcpy (self->needle, (host), size); \
    self->needle += size; \
}

//  Get a block of octets from the frame
#define GET_OCTETS(host,size) { \
    if (self->needle + size > self->ceiling) \
        goto malformed; \
    memcpy ((host), self->needle, size); \
    self->needle += size; \
}

//  Put a 1-byte number to the frame
#define PUT_NUMBER1(host) { \
    *(byte *) self->needle = (host); \
    self->needle++; \
}

//  Put a 2-byte number to the frame
#define PUT_NUMBER2(host) { \
    self->needle [0] = (byte) (((host) >> 8)  & 255); \
    self->needle [1] = (byte) (((host))       & 255); \
    self->needle += 2; \
}

//  Put a 4-byte number to the frame
#define PUT_NUMBER4(host) { \
    self->needle [0] = (byte) (((host) >> 24) & 255); \
    self->needle [1] = (byte) (((host) >> 16) & 255); \
    self->needle [2] = (byte) (((host) >> 8)  & 255); \
    self->needle [3] = (byte) (((host))       & 255); \
    self->needle += 4; \
}

//  Put a 8-byte number to the frame
#define PUT_NUMBER8(host) { \
    self->needle [0] = (byte) (((host) >> 56) & 255); \
    self->needle [1] = (byte) (((host) >> 48) & 255); \
    self->needle [2] = (byte) (((host) >> 40) & 255); \
    self->needle [3] = (byte) (((host) >> 32) & 255); \
    self->needle [4] = (byte) (((host) >> 24) & 255); \
    self->needle [5] = (byte) (((host) >> 16) & 255); \
    self->needle [6] = (byte) (((host) >> 8)  & 255); \
    self->needle [7] = (byte) (((host))       & 255); \
    self->needle += 8; \
}

//  Get a 1-byte number from the frame
#define GET_NUMBER1(host) { \
    if (self->needle + 1 > self->ceiling) \
        goto malformed; \
    (host) = *(byte *) self->needle; \
    self->needle++; \
}

//  Get a 2-byte number from the frame
#define GET_NUMBER2(host) { \
    if (self->needle + 2 > self->ceiling) \
        goto malformed; \
    (host) = ((uint16_t) (self->needle [0]) << 8) \
           +  (uint16_t) (self->needle [1]); \
    self->needle += 2; \
}

//  Get a 4-byte number from the frame
#define GET_NUMBER4(host) { \
    if (self->needle + 4 > self->ceiling) \
        goto malformed; \
    (host) = ((uint32_t) (self->needle [0]) << 24) \
           + ((uint32_t) (self->needle [1]) << 16) \
           + ((uint32_t) (self->needle [2]) << 8) \
           +  (uint32_t) (self->needle [3]); \
    self->needle += 4; \
}

//  Get a 8-byte number from the frame
#define GET_NUMBER8(host) { \
    if (self->needle + 8 > self->ceiling) \
        goto malformed; \
    (host) = ((uint64_t) (self->needle [0]) << 56) \
           + ((uint64_t) (self->needle [1]) << 48) \
           + ((uint64_t) (self->needle [2]) << 40) \
           + ((uint64_t) (self->needle [3]) << 32) \
           + ((uint64_t) (self->needle [4]) << 24) \
           + ((uint64_t) (self->needle [5]) << 16) \
           + ((uint64_t) (self->needle [6]) << 8) \
           +  (uint64_t) (self->needle [7]); \
    self->needle += 8; \
}

//  Put a string to the frame
#define PUT_STRING(host) { \
    size_t string_size = strlen (host); \
    PUT_NUMBER1 (string_size); \
    memcpy (self->needle, (host), string_size); \
    self->needle += string_size; \
}

//  Get a string from the frame
#define GET_STRING(host) { \
    size_t string_size; \
    GET_NUMBER1 (string_size); \
    if (self->needle + string_size > (self->ceiling)) \
        goto malformed; \
    (host) = (char *) malloc (string_size + 1); \
    memcpy ((host), self->needle, string_size); \
    (host) [string_size] = 0; \
    self->needle += string_size; \
}

//  Put a long string to the frame
#define PUT_LONGSTR(host) { \
    size_t string_size = strlen (host); \
    PUT_NUMBER4 (string_size); \
    memcpy (self->needle, (host), string_size); \
    self->needle += string_size; \
}

//  Get a long string from the frame
#define GET_LONGSTR(host) { \
    size_t string_size; \
    GET_NUMBER4 (string_size); \
    if (self->needle + string_size > (self->ceiling)) \
        goto malformed; \
    (host) = (char *) malloc (string_size + 1); \
    memcpy ((host), self->needle, string_size); \
    (host) [string_size] = 0; \
    self->needle += string_size; \
}


//  --------------------------------------------------------------------------
//  Create a new zre_msg

zre_msg_t *
zre_msg_new (int id)
{
    zre_msg_t *self = (zre_msg_t *) zmalloc (sizeof (zre_msg_t));
    self->id = id;
    return self;
}


//  --------------------------------------------------------------------------
//  Destroy the zre_msg

void
zre_msg_destroy (zre_msg_t **self_p)
{
    assert (self_p);
    if (*self_p) {
        zre_msg_t *self = *self_p;

        //  Free class properties
        zframe_destroy (&self->routing_id);
        free (self->ipaddress);
        if (self->groups)
            zlist_destroy (&self->groups);
        zhash_destroy (&self->headers);
        zmsg_destroy (&self->content);
        free (self->group);

        //  Free object itself
        free (self);
        *self_p = NULL;
    }
}


//  --------------------------------------------------------------------------
//  Parse a zre_msg from zmsg_t. Returns a new object, or NULL if
//  the message could not be parsed, or was NULL. If the socket type is
//  ZMQ_ROUTER, then parses the first frame as a routing_id. Destroys msg
//  and nullifies the msg refernce.

zre_msg_t *
zre_msg_decode (zmsg_t **msg_p, int socket_type)
{
    assert (msg_p);
    zmsg_t *msg = *msg_p;
    if (msg == NULL)
        return NULL;
        
    zre_msg_t *self = zre_msg_new (0);
    //  If message came from a router socket, first frame is routing_id
    if (socket_type == ZMQ_ROUTER) {
        self->routing_id = zmsg_pop (msg);
        //  If message was not valid, forget about it
        if (!self->routing_id || !zmsg_next (msg)) {
            zre_msg_destroy (&self);
            return (NULL);      //  Malformed or empty
        }
    }
    //  Read and parse command in frame
    zframe_t *frame = zmsg_pop (msg);
    if (!frame) 
        goto empty;             //  Malformed or empty

    //  Get and check protocol signature
    self->needle = zframe_data (frame);
    self->ceiling = self->needle + zframe_size (frame);
    uint16_t signature;
    GET_NUMBER2 (signature);
    if (signature != (0xAAA0 | 1))
        goto empty;             //  Invalid signature

    //  Get message id and parse per message type
    GET_NUMBER1 (self->id);

    switch (self->id) {
        case ZRE_MSG_HELLO:
            GET_NUMBER2 (self->sequence);
            GET_STRING (self->ipaddress);
            GET_NUMBER2 (self->mailbox);
            {
                size_t list_size;
                GET_NUMBER4 (list_size);
                self->groups = zlist_new ();
                zlist_autofree (self->groups);
                while (list_size--) {
                    char *string;
                    GET_LONGSTR (string);
                    zlist_append (self->groups, string);
                    free (string);
                }
            }
            GET_NUMBER1 (self->status);
            {
                size_t hash_size;
                GET_NUMBER4 (hash_size);
                self->headers = zhash_new ();
                zhash_autofree (self->headers);
                while (hash_size--) {
                    char *key, *value;
                    GET_STRING (key);
                    GET_LONGSTR (value);
                    zhash_insert (self->headers, key, value);
                    free (key);
                    free (value);
                }
            }
            break;

        case ZRE_MSG_WHISPER:
            GET_NUMBER2 (self->sequence);
            //  Get zero or more remaining frames, leaving current
            //  frame untouched
            self->content = zmsg_new ();
            while (zmsg_size (msg))
                zmsg_add (self->content, zmsg_pop (msg));
            break;

        case ZRE_MSG_SHOUT:
            GET_NUMBER2 (self->sequence);
            GET_STRING (self->group);
            //  Get zero or more remaining frames, leaving current
            //  frame untouched
            self->content = zmsg_new ();
            while (zmsg_size (msg))
                zmsg_add (self->content, zmsg_pop (msg));
            break;

        case ZRE_MSG_JOIN:
            GET_NUMBER2 (self->sequence);
            GET_STRING (self->group);
            GET_NUMBER1 (self->status);
            break;

        case ZRE_MSG_LEAVE:
            GET_NUMBER2 (self->sequence);
            GET_STRING (self->group);
            GET_NUMBER1 (self->status);
            break;

        case ZRE_MSG_PING:
            GET_NUMBER2 (self->sequence);
            break;

        case ZRE_MSG_PING_OK:
            GET_NUMBER2 (self->sequence);
            break;

        default:
            goto malformed;
    }
    //  Successful return
    zframe_destroy (&frame);
    zmsg_destroy (msg_p);
    return self;

    //  Error returns
    malformed:
        printf ("E: malformed message '%d'\n", self->id);
    empty:
        zframe_destroy (&frame);
        zmsg_destroy (msg_p);
        zre_msg_destroy (&self);
        return (NULL);
}


//  --------------------------------------------------------------------------
//  Receive and parse a zre_msg from the socket. Returns new object or
//  NULL if error. Will block if there's no message waiting.

zre_msg_t *
zre_msg_recv (void *input)
{
    assert (input);
    zmsg_t *msg = zmsg_recv (input);
    return zre_msg_decode (&msg, zsocket_type (input));
}


//  --------------------------------------------------------------------------
//  Receive and parse a zre_msg from the socket. Returns new object, 
//  or NULL either if there was no input waiting, or the recv was interrupted.

zre_msg_t *
zre_msg_recv_nowait (void *input)
{
    assert (input);
    zmsg_t *msg = zmsg_recv_nowait (input);
    return zre_msg_decode (&msg, zsocket_type (input));
}


//  Count size of key/value pair for serialization
//  Key is encoded as string, value as longstr
static int
s_headers_count (const char *key, void *item, void *argument)
{
    zre_msg_t *self = (zre_msg_t *) argument;
    self->headers_bytes += 1 + strlen (key) + 4 + strlen ((char *) item);
    return 0;
}

//  Serialize headers key=value pair
static int
s_headers_write (const char *key, void *item, void *argument)
{
    zre_msg_t *self = (zre_msg_t *) argument;
    PUT_STRING (key);
    PUT_LONGSTR ((char *) item);
    return 0;
}


//  Encode zre_msg into zmsg and destroy it. Returns a newly created
//  object or NULL if error. Use when not in control of sending the message.
//  If the socket_type is ZMQ_ROUTER, then stores the routing_id as the
//  first frame of the resulting message.

zmsg_t *
zre_msg_encode (zre_msg_t *self, int socket_type)
{
    assert (self);
    zmsg_t *msg = zmsg_new ();

    //  If we're sending to a ROUTER, send the routing_id first
    if (socket_type == ZMQ_ROUTER)
        zmsg_prepend (msg, &self->routing_id);
        
    size_t frame_size = 2 + 1;          //  Signature and message ID
    switch (self->id) {
        case ZRE_MSG_HELLO:
            //  sequence is a 2-byte integer
            frame_size += 2;
            //  ipaddress is a string with 1-byte length
            frame_size++;       //  Size is one octet
            if (self->ipaddress)
                frame_size += strlen (self->ipaddress);
            //  mailbox is a 2-byte integer
            frame_size += 2;
            //  groups is an array of strings
            frame_size += 4;    //  Size is 4 octets
            if (self->groups) {
                //  Add up size of list contents
                char *groups = (char *) zlist_first (self->groups);
                while (groups) {
                    frame_size += 4 + strlen (groups);
                    groups = (char *) zlist_next (self->groups);
                }
            }
            //  status is a 1-byte integer
            frame_size += 1;
            //  headers is an array of key=value strings
            frame_size += 4;    //  Size is 4 octets
            if (self->headers) {
                self->headers_bytes = 0;
                //  Add up size of dictionary contents
                zhash_foreach (self->headers, s_headers_count, self);
            }
            frame_size += self->headers_bytes;
            break;
            
        case ZRE_MSG_WHISPER:
            //  sequence is a 2-byte integer
            frame_size += 2;
            break;
            
        case ZRE_MSG_SHOUT:
            //  sequence is a 2-byte integer
            frame_size += 2;
            //  group is a string with 1-byte length
            frame_size++;       //  Size is one octet
            if (self->group)
                frame_size += strlen (self->group);
            break;
            
        case ZRE_MSG_JOIN:
            //  sequence is a 2-byte integer
            frame_size += 2;
            //  group is a string with 1-byte length
            frame_size++;       //  Size is one octet
            if (self->group)
                frame_size += strlen (self->group);
            //  status is a 1-byte integer
            frame_size += 1;
            break;
            
        case ZRE_MSG_LEAVE:
            //  sequence is a 2-byte integer
            frame_size += 2;
            //  group is a string with 1-byte length
            frame_size++;       //  Size is one octet
            if (self->group)
                frame_size += strlen (self->group);
            //  status is a 1-byte integer
            frame_size += 1;
            break;
            
        case ZRE_MSG_PING:
            //  sequence is a 2-byte integer
            frame_size += 2;
            break;
            
        case ZRE_MSG_PING_OK:
            //  sequence is a 2-byte integer
            frame_size += 2;
            break;
            
        default:
            printf ("E: bad message type '%d', not sent\n", self->id);
            //  No recovery, this is a fatal application error
            assert (false);
    }
    //  Now serialize message into the frame
    zframe_t *frame = zframe_new (NULL, frame_size);
    self->needle = zframe_data (frame);
    PUT_NUMBER2 (0xAAA0 | 1);
    PUT_NUMBER1 (self->id);

    switch (self->id) {
        case ZRE_MSG_HELLO:
            PUT_NUMBER2 (self->sequence);
            if (self->ipaddress) {
                PUT_STRING (self->ipaddress);
            }
            else
                PUT_NUMBER1 (0);    //  Empty string
            PUT_NUMBER2 (self->mailbox);
            if (self->groups) {
                PUT_NUMBER4 (zlist_size (self->groups));
                char *groups = (char *) zlist_first (self->groups);
                while (groups) {
                    PUT_LONGSTR (groups);
                    groups = (char *) zlist_next (self->groups);
                }
            }
            else
                PUT_NUMBER4 (0);    //  Empty string array
            PUT_NUMBER1 (self->status);
            if (self->headers) {
                PUT_NUMBER4 (zhash_size (self->headers));
                zhash_foreach (self->headers, s_headers_write, self);
            }
            else
                PUT_NUMBER4 (0);    //  Empty dictionary
            break;

        case ZRE_MSG_WHISPER:
            PUT_NUMBER2 (self->sequence);
            break;

        case ZRE_MSG_SHOUT:
            PUT_NUMBER2 (self->sequence);
            if (self->group) {
                PUT_STRING (self->group);
            }
            else
                PUT_NUMBER1 (0);    //  Empty string
            break;

        case ZRE_MSG_JOIN:
            PUT_NUMBER2 (self->sequence);
            if (self->group) {
                PUT_STRING (self->group);
            }
            else
                PUT_NUMBER1 (0);    //  Empty string
            PUT_NUMBER1 (self->status);
            break;

        case ZRE_MSG_LEAVE:
            PUT_NUMBER2 (self->sequence);
            if (self->group) {
                PUT_STRING (self->group);
            }
            else
                PUT_NUMBER1 (0);    //  Empty string
            PUT_NUMBER1 (self->status);
            break;

        case ZRE_MSG_PING:
            PUT_NUMBER2 (self->sequence);
            break;

        case ZRE_MSG_PING_OK:
            PUT_NUMBER2 (self->sequence);
            break;

    }
    //  Now send the data frame
    if (zmsg_append (msg, &frame)) {
        zmsg_destroy (&msg);
        zre_msg_destroy (&self);
        return NULL;
    }
    //  Now send the content field if set
    if (self->id == ZRE_MSG_WHISPER) {
        zframe_t *content_part = zmsg_pop (self->content);
        while (content_part) {
            zmsg_append (msg, &content_part);
            content_part = zmsg_pop (self->content);
        }
    }
    //  Now send the content field if set
    if (self->id == ZRE_MSG_SHOUT) {
        zframe_t *content_part = zmsg_pop (self->content);
        while (content_part) {
            zmsg_append (msg, &content_part);
            content_part = zmsg_pop (self->content);
        }
    }
    //  Destroy zre_msg object
    zre_msg_destroy (&self);
    return msg;

}

//  --------------------------------------------------------------------------
//  Send the zre_msg to the socket, and destroy it
//  Returns 0 if OK, else -1

int
zre_msg_send (zre_msg_t **self_p, void *output)
{
    assert (self_p);
    assert (*self_p);
    assert (output);

    zre_msg_t *self = *self_p;
    zmsg_t *msg = zre_msg_encode (self, zsocket_type (output));
    if (msg && zmsg_send (&msg, output) == 0)
        return 0;
    else
        return -1;              //  Failed to encode, or send
}


//  --------------------------------------------------------------------------
//  Send the zre_msg to the output, and do not destroy it

int
zre_msg_send_again (zre_msg_t *self, void *output)
{
    assert (self);
    assert (output);
    self = zre_msg_dup (self);
    return zre_msg_send (&self, output);
}


//  --------------------------------------------------------------------------
//  Send the HELLO to the socket in one step

int
zre_msg_send_hello (
    void *output,
    uint16_t sequence,
    const char *ipaddress,
    uint16_t mailbox,
    zlist_t *groups,
    byte status,
    zhash_t *headers)
{
    zre_msg_t *self = zre_msg_new (ZRE_MSG_HELLO);
    zre_msg_set_sequence (self, sequence);
    zre_msg_set_ipaddress (self, ipaddress);
    zre_msg_set_mailbox (self, mailbox);
    zlist_t *groups_copy = zlist_dup (groups);
    zre_msg_set_groups (self, &groups_copy);
    zre_msg_set_status (self, status);
    zhash_t *headers_copy = zhash_dup (headers);
    zre_msg_set_headers (self, &headers_copy);
    return zre_msg_send (&self, output);
}


//  --------------------------------------------------------------------------
//  Send the WHISPER to the socket in one step

int
zre_msg_send_whisper (
    void *output,
    uint16_t sequence,
    zmsg_t *content)
{
    zre_msg_t *self = zre_msg_new (ZRE_MSG_WHISPER);
    zre_msg_set_sequence (self, sequence);
    zmsg_t *content_copy = zmsg_dup (content);
    zre_msg_set_content (self, &content_copy);
    return zre_msg_send (&self, output);
}


//  --------------------------------------------------------------------------
//  Send the SHOUT to the socket in one step

int
zre_msg_send_shout (
    void *output,
    uint16_t sequence,
    const char *group,
    zmsg_t *content)
{
    zre_msg_t *self = zre_msg_new (ZRE_MSG_SHOUT);
    zre_msg_set_sequence (self, sequence);
    zre_msg_set_group (self, group);
    zmsg_t *content_copy = zmsg_dup (content);
    zre_msg_set_content (self, &content_copy);
    return zre_msg_send (&self, output);
}


//  --------------------------------------------------------------------------
//  Send the JOIN to the socket in one step

int
zre_msg_send_join (
    void *output,
    uint16_t sequence,
    const char *group,
    byte status)
{
    zre_msg_t *self = zre_msg_new (ZRE_MSG_JOIN);
    zre_msg_set_sequence (self, sequence);
    zre_msg_set_group (self, group);
    zre_msg_set_status (self, status);
    return zre_msg_send (&self, output);
}


//  --------------------------------------------------------------------------
//  Send the LEAVE to the socket in one step

int
zre_msg_send_leave (
    void *output,
    uint16_t sequence,
    const char *group,
    byte status)
{
    zre_msg_t *self = zre_msg_new (ZRE_MSG_LEAVE);
    zre_msg_set_sequence (self, sequence);
    zre_msg_set_group (self, group);
    zre_msg_set_status (self, status);
    return zre_msg_send (&self, output);
}


//  --------------------------------------------------------------------------
//  Send the PING to the socket in one step

int
zre_msg_send_ping (
    void *output,
    uint16_t sequence)
{
    zre_msg_t *self = zre_msg_new (ZRE_MSG_PING);
    zre_msg_set_sequence (self, sequence);
    return zre_msg_send (&self, output);
}


//  --------------------------------------------------------------------------
//  Send the PING_OK to the socket in one step

int
zre_msg_send_ping_ok (
    void *output,
    uint16_t sequence)
{
    zre_msg_t *self = zre_msg_new (ZRE_MSG_PING_OK);
    zre_msg_set_sequence (self, sequence);
    return zre_msg_send (&self, output);
}


//  --------------------------------------------------------------------------
//  Duplicate the zre_msg message

zre_msg_t *
zre_msg_dup (zre_msg_t *self)
{
    if (!self)
        return NULL;
        
    zre_msg_t *copy = zre_msg_new (self->id);
    if (self->routing_id)
        copy->routing_id = zframe_dup (self->routing_id);
    switch (self->id) {
        case ZRE_MSG_HELLO:
            copy->sequence = self->sequence;
            copy->ipaddress = self->ipaddress? strdup (self->ipaddress): NULL;
            copy->mailbox = self->mailbox;
            copy->groups = self->groups? zlist_dup (self->groups): NULL;
            copy->status = self->status;
            copy->headers = self->headers? zhash_dup (self->headers): NULL;
            break;

        case ZRE_MSG_WHISPER:
            copy->sequence = self->sequence;
            copy->content = self->content? zmsg_dup (self->content): NULL;
            break;

        case ZRE_MSG_SHOUT:
            copy->sequence = self->sequence;
            copy->group = self->group? strdup (self->group): NULL;
            copy->content = self->content? zmsg_dup (self->content): NULL;
            break;

        case ZRE_MSG_JOIN:
            copy->sequence = self->sequence;
            copy->group = self->group? strdup (self->group): NULL;
            copy->status = self->status;
            break;

        case ZRE_MSG_LEAVE:
            copy->sequence = self->sequence;
            copy->group = self->group? strdup (self->group): NULL;
            copy->status = self->status;
            break;

        case ZRE_MSG_PING:
            copy->sequence = self->sequence;
            break;

        case ZRE_MSG_PING_OK:
            copy->sequence = self->sequence;
            break;

    }
    return copy;
}


//  Dump headers key=value pair to stdout
static int
s_headers_dump (const char *key, void *item, void *argument)
{
    printf ("        %s=%s\n", key, (char *) item);
    return 0;
}


//  --------------------------------------------------------------------------
//  Print contents of message to stdout

void
zre_msg_dump (zre_msg_t *self)
{
    assert (self);
    switch (self->id) {
        case ZRE_MSG_HELLO:
            puts ("HELLO:");
            printf ("    sequence=%ld\n", (long) self->sequence);
            if (self->ipaddress)
                printf ("    ipaddress='%s'\n", self->ipaddress);
            else
                printf ("    ipaddress=\n");
            printf ("    mailbox=%ld\n", (long) self->mailbox);
            printf ("    groups={");
            if (self->groups) {
                char *groups = (char *) zlist_first (self->groups);
                while (groups) {
                    printf (" '%s'", groups);
                    groups = (char *) zlist_next (self->groups);
                }
            }
            printf (" }\n");
            printf ("    status=%ld\n", (long) self->status);
            printf ("    headers={\n");
            if (self->headers)
                zhash_foreach (self->headers, s_headers_dump, self);
            else
                printf ("(NULL)\n");
            printf ("    }\n");
            break;
            
        case ZRE_MSG_WHISPER:
            puts ("WHISPER:");
            printf ("    sequence=%ld\n", (long) self->sequence);
            printf ("    content={\n");
            if (self->content)
                zmsg_dump (self->content);
            else
                printf ("(NULL)\n");
            printf ("    }\n");
            break;
            
        case ZRE_MSG_SHOUT:
            puts ("SHOUT:");
            printf ("    sequence=%ld\n", (long) self->sequence);
            if (self->group)
                printf ("    group='%s'\n", self->group);
            else
                printf ("    group=\n");
            printf ("    content={\n");
            if (self->content)
                zmsg_dump (self->content);
            else
                printf ("(NULL)\n");
            printf ("    }\n");
            break;
            
        case ZRE_MSG_JOIN:
            puts ("JOIN:");
            printf ("    sequence=%ld\n", (long) self->sequence);
            if (self->group)
                printf ("    group='%s'\n", self->group);
            else
                printf ("    group=\n");
            printf ("    status=%ld\n", (long) self->status);
            break;
            
        case ZRE_MSG_LEAVE:
            puts ("LEAVE:");
            printf ("    sequence=%ld\n", (long) self->sequence);
            if (self->group)
                printf ("    group='%s'\n", self->group);
            else
                printf ("    group=\n");
            printf ("    status=%ld\n", (long) self->status);
            break;
            
        case ZRE_MSG_PING:
            puts ("PING:");
            printf ("    sequence=%ld\n", (long) self->sequence);
            break;
            
        case ZRE_MSG_PING_OK:
            puts ("PING_OK:");
            printf ("    sequence=%ld\n", (long) self->sequence);
            break;
            
    }
}


//  --------------------------------------------------------------------------
//  Get/set the message routing_id

zframe_t *
zre_msg_routing_id (zre_msg_t *self)
{
    assert (self);
    return self->routing_id;
}

void
zre_msg_set_routing_id (zre_msg_t *self, zframe_t *routing_id)
{
    if (self->routing_id)
        zframe_destroy (&self->routing_id);
    self->routing_id = zframe_dup (routing_id);
}


//  --------------------------------------------------------------------------
//  Get/set the zre_msg id

int
zre_msg_id (zre_msg_t *self)
{
    assert (self);
    return self->id;
}

void
zre_msg_set_id (zre_msg_t *self, int id)
{
    self->id = id;
}

//  --------------------------------------------------------------------------
//  Return a printable command string

const char *
zre_msg_command (zre_msg_t *self)
{
    assert (self);
    switch (self->id) {
        case ZRE_MSG_HELLO:
            return ("HELLO");
            break;
        case ZRE_MSG_WHISPER:
            return ("WHISPER");
            break;
        case ZRE_MSG_SHOUT:
            return ("SHOUT");
            break;
        case ZRE_MSG_JOIN:
            return ("JOIN");
            break;
        case ZRE_MSG_LEAVE:
            return ("LEAVE");
            break;
        case ZRE_MSG_PING:
            return ("PING");
            break;
        case ZRE_MSG_PING_OK:
            return ("PING_OK");
            break;
    }
    return "?";
}

//  --------------------------------------------------------------------------
//  Get/set the sequence field

uint16_t
zre_msg_sequence (zre_msg_t *self)
{
    assert (self);
    return self->sequence;
}

void
zre_msg_set_sequence (zre_msg_t *self, uint16_t sequence)
{
    assert (self);
    self->sequence = sequence;
}


//  --------------------------------------------------------------------------
//  Get/set the ipaddress field

const char *
zre_msg_ipaddress (zre_msg_t *self)
{
    assert (self);
    return self->ipaddress;
}

void
zre_msg_set_ipaddress (zre_msg_t *self, const char *format, ...)
{
    //  Format ipaddress from provided arguments
    assert (self);
    va_list argptr;
    va_start (argptr, format);
    free (self->ipaddress);
    self->ipaddress = zsys_vprintf (format, argptr);
    va_end (argptr);
}


//  --------------------------------------------------------------------------
//  Get/set the mailbox field

uint16_t
zre_msg_mailbox (zre_msg_t *self)
{
    assert (self);
    return self->mailbox;
}

void
zre_msg_set_mailbox (zre_msg_t *self, uint16_t mailbox)
{
    assert (self);
    self->mailbox = mailbox;
}


//  --------------------------------------------------------------------------
//  Get the groups field, without transferring ownership

zlist_t *
zre_msg_groups (zre_msg_t *self)
{
    assert (self);
    return self->groups;
}

//  Get the groups field and transfer ownership to caller

zlist_t *
zre_msg_get_groups (zre_msg_t *self)
{
    assert (self);
    zlist_t *groups = self->groups;
    self->groups = NULL;
    return groups;
}

//  Set the groups field, transferring ownership from caller

void
zre_msg_set_groups (zre_msg_t *self, zlist_t **groups_p)
{
    assert (self);
    assert (groups_p);
    zlist_destroy (&self->groups);
    self->groups = *groups_p;
    *groups_p = NULL;
}

//  --------------------------------------------------------------------------
//  Iterate through the groups field, and append a groups value

const char *
zre_msg_groups_first (zre_msg_t *self)
{
    assert (self);
    if (self->groups)
        return (char *) (zlist_first (self->groups));
    else
        return NULL;
}

const char *
zre_msg_groups_next (zre_msg_t *self)
{
    assert (self);
    if (self->groups)
        return (char *) (zlist_next (self->groups));
    else
        return NULL;
}

void
zre_msg_groups_append (zre_msg_t *self, const char *format, ...)
{
    //  Format into newly allocated string
    assert (self);
    va_list argptr;
    va_start (argptr, format);
    char *string = zsys_vprintf (format, argptr);
    va_end (argptr);

    //  Attach string to list
    if (!self->groups) {
        self->groups = zlist_new ();
        zlist_autofree (self->groups);
    }
    zlist_append (self->groups, string);
    free (string);
}

size_t
zre_msg_groups_size (zre_msg_t *self)
{
    return zlist_size (self->groups);
}


//  --------------------------------------------------------------------------
//  Get/set the status field

byte
zre_msg_status (zre_msg_t *self)
{
    assert (self);
    return self->status;
}

void
zre_msg_set_status (zre_msg_t *self, byte status)
{
    assert (self);
    self->status = status;
}


//  --------------------------------------------------------------------------
//  Get the headers field without transferring ownership

zhash_t *
zre_msg_headers (zre_msg_t *self)
{
    assert (self);
    return self->headers;
}

//  Get the headers field and transfer ownership to caller

zhash_t *
zre_msg_get_headers (zre_msg_t *self)
{
    zhash_t *headers = self->headers;
    self->headers = NULL;
    return headers;
}

//  Set the headers field, transferring ownership from caller

void
zre_msg_set_headers (zre_msg_t *self, zhash_t **headers_p)
{
    assert (self);
    assert (headers_p);
    zhash_destroy (&self->headers);
    self->headers = *headers_p;
    *headers_p = NULL;
}

//  --------------------------------------------------------------------------
//  Get/set a value in the headers dictionary

const char *
zre_msg_headers_string (zre_msg_t *self, const char *key, const char *default_value)
{
    assert (self);
    const char *value = NULL;
    if (self->headers)
        value = (const char *) (zhash_lookup (self->headers, key));
    if (!value)
        value = default_value;

    return value;
}

uint64_t
zre_msg_headers_number (zre_msg_t *self, const char *key, uint64_t default_value)
{
    assert (self);
    uint64_t value = default_value;
    char *string = NULL;
    if (self->headers)
        string = (char *) (zhash_lookup (self->headers, key));
    if (string)
        value = atol (string);

    return value;
}

void
zre_msg_headers_insert (zre_msg_t *self, const char *key, const char *format, ...)
{
    //  Format into newly allocated string
    assert (self);
    va_list argptr;
    va_start (argptr, format);
    char *string = zsys_vprintf (format, argptr);
    va_end (argptr);

    //  Store string in hash table
    if (!self->headers) {
        self->headers = zhash_new ();
        zhash_autofree (self->headers);
    }
    zhash_update (self->headers, key, string);
    free (string);
}

size_t
zre_msg_headers_size (zre_msg_t *self)
{
    return zhash_size (self->headers);
}


//  --------------------------------------------------------------------------
//  Get the content field without transferring ownership

zmsg_t *
zre_msg_content (zre_msg_t *self)
{
    assert (self);
    return self->content;
}

//  Get the content field and transfer ownership to caller

zmsg_t *
zre_msg_get_content (zre_msg_t *self)
{
    zmsg_t *content = self->content;
    self->content = NULL;
    return content;
}

//  Set the content field, transferring ownership from caller

void
zre_msg_set_content (zre_msg_t *self, zmsg_t **msg_p)
{
    assert (self);
    assert (msg_p);
    zmsg_destroy (&self->content);
    self->content = *msg_p;
    *msg_p = NULL;
}


//  --------------------------------------------------------------------------
//  Get/set the group field

const char *
zre_msg_group (zre_msg_t *self)
{
    assert (self);
    return self->group;
}

void
zre_msg_set_group (zre_msg_t *self, const char *format, ...)
{
    //  Format group from provided arguments
    assert (self);
    va_list argptr;
    va_start (argptr, format);
    free (self->group);
    self->group = zsys_vprintf (format, argptr);
    va_end (argptr);
}



//  --------------------------------------------------------------------------
//  Selftest

int
zre_msg_test (bool verbose)
{
    printf (" * zre_msg: ");

    //  @selftest
    //  Simple create/destroy test
    zre_msg_t *self = zre_msg_new (0);
    assert (self);
    zre_msg_destroy (&self);

    //  Create pair of sockets we can send through
    zctx_t *ctx = zctx_new ();
    assert (ctx);

    void *output = zsocket_new (ctx, ZMQ_DEALER);
    assert (output);
    zsocket_bind (output, "inproc://selftest");

    void *input = zsocket_new (ctx, ZMQ_ROUTER);
    assert (input);
    zsocket_connect (input, "inproc://selftest");
    
    //  Encode/send/decode and verify each message type
    int instance;
    zre_msg_t *copy;
    self = zre_msg_new (ZRE_MSG_HELLO);
    
    //  Check that _dup works on empty message
    copy = zre_msg_dup (self);
    assert (copy);
    zre_msg_destroy (&copy);

    zre_msg_set_sequence (self, 123);
    zre_msg_set_ipaddress (self, "Life is short but Now lasts for ever");
    zre_msg_set_mailbox (self, 123);
    zre_msg_groups_append (self, "Name: %s", "Brutus");
    zre_msg_groups_append (self, "Age: %d", 43);
    zre_msg_set_status (self, 123);
    zre_msg_headers_insert (self, "Name", "Brutus");
    zre_msg_headers_insert (self, "Age", "%d", 43);
    //  Send twice from same object
    zre_msg_send_again (self, output);
    zre_msg_send (&self, output);

    for (instance = 0; instance < 2; instance++) {
        self = zre_msg_recv (input);
        assert (self);
        assert (zre_msg_routing_id (self));
        
        assert (zre_msg_sequence (self) == 123);
        assert (streq (zre_msg_ipaddress (self), "Life is short but Now lasts for ever"));
        assert (zre_msg_mailbox (self) == 123);
        assert (zre_msg_groups_size (self) == 2);
        assert (streq (zre_msg_groups_first (self), "Name: Brutus"));
        assert (streq (zre_msg_groups_next (self), "Age: 43"));
        assert (zre_msg_status (self) == 123);
        assert (zre_msg_headers_size (self) == 2);
        assert (streq (zre_msg_headers_string (self, "Name", "?"), "Brutus"));
        assert (zre_msg_headers_number (self, "Age", 0) == 43);
        zre_msg_destroy (&self);
    }
    self = zre_msg_new (ZRE_MSG_WHISPER);
    
    //  Check that _dup works on empty message
    copy = zre_msg_dup (self);
    assert (copy);
    zre_msg_destroy (&copy);

    zre_msg_set_sequence (self, 123);
    zmsg_t *whisper_content = zmsg_new ();
    zre_msg_set_content (self, &whisper_content);
    zmsg_addstr (zre_msg_content (self), "Hello, World");
    //  Send twice from same object
    zre_msg_send_again (self, output);
    zre_msg_send (&self, output);

    for (instance = 0; instance < 2; instance++) {
        self = zre_msg_recv (input);
        assert (self);
        assert (zre_msg_routing_id (self));
        
        assert (zre_msg_sequence (self) == 123);
        assert (zmsg_size (zre_msg_content (self)) == 1);
        zre_msg_destroy (&self);
    }
    self = zre_msg_new (ZRE_MSG_SHOUT);
    
    //  Check that _dup works on empty message
    copy = zre_msg_dup (self);
    assert (copy);
    zre_msg_destroy (&copy);

    zre_msg_set_sequence (self, 123);
    zre_msg_set_group (self, "Life is short but Now lasts for ever");
    zmsg_t *shout_content = zmsg_new ();
    zre_msg_set_content (self, &shout_content);
    zmsg_addstr (zre_msg_content (self), "Hello, World");
    //  Send twice from same object
    zre_msg_send_again (self, output);
    zre_msg_send (&self, output);

    for (instance = 0; instance < 2; instance++) {
        self = zre_msg_recv (input);
        assert (self);
        assert (zre_msg_routing_id (self));
        
        assert (zre_msg_sequence (self) == 123);
        assert (streq (zre_msg_group (self), "Life is short but Now lasts for ever"));
        assert (zmsg_size (zre_msg_content (self)) == 1);
        zre_msg_destroy (&self);
    }
    self = zre_msg_new (ZRE_MSG_JOIN);
    
    //  Check that _dup works on empty message
    copy = zre_msg_dup (self);
    assert (copy);
    zre_msg_destroy (&copy);

    zre_msg_set_sequence (self, 123);
    zre_msg_set_group (self, "Life is short but Now lasts for ever");
    zre_msg_set_status (self, 123);
    //  Send twice from same object
    zre_msg_send_again (self, output);
    zre_msg_send (&self, output);

    for (instance = 0; instance < 2; instance++) {
        self = zre_msg_recv (input);
        assert (self);
        assert (zre_msg_routing_id (self));
        
        assert (zre_msg_sequence (self) == 123);
        assert (streq (zre_msg_group (self), "Life is short but Now lasts for ever"));
        assert (zre_msg_status (self) == 123);
        zre_msg_destroy (&self);
    }
    self = zre_msg_new (ZRE_MSG_LEAVE);
    
    //  Check that _dup works on empty message
    copy = zre_msg_dup (self);
    assert (copy);
    zre_msg_destroy (&copy);

    zre_msg_set_sequence (self, 123);
    zre_msg_set_group (self, "Life is short but Now lasts for ever");
    zre_msg_set_status (self, 123);
    //  Send twice from same object
    zre_msg_send_again (self, output);
    zre_msg_send (&self, output);

    for (instance = 0; instance < 2; instance++) {
        self = zre_msg_recv (input);
        assert (self);
        assert (zre_msg_routing_id (self));
        
        assert (zre_msg_sequence (self) == 123);
        assert (streq (zre_msg_group (self), "Life is short but Now lasts for ever"));
        assert (zre_msg_status (self) == 123);
        zre_msg_destroy (&self);
    }
    self = zre_msg_new (ZRE_MSG_PING);
    
    //  Check that _dup works on empty message
    copy = zre_msg_dup (self);
    assert (copy);
    zre_msg_destroy (&copy);

    zre_msg_set_sequence (self, 123);
    //  Send twice from same object
    zre_msg_send_again (self, output);
    zre_msg_send (&self, output);

    for (instance = 0; instance < 2; instance++) {
        self = zre_msg_recv (input);
        assert (self);
        assert (zre_msg_routing_id (self));
        
        assert (zre_msg_sequence (self) == 123);
        zre_msg_destroy (&self);
    }
    self = zre_msg_new (ZRE_MSG_PING_OK);
    
    //  Check that _dup works on empty message
    copy = zre_msg_dup (self);
    assert (copy);
    zre_msg_destroy (&copy);

    zre_msg_set_sequence (self, 123);
    //  Send twice from same object
    zre_msg_send_again (self, output);
    zre_msg_send (&self, output);

    for (instance = 0; instance < 2; instance++) {
        self = zre_msg_recv (input);
        assert (self);
        assert (zre_msg_routing_id (self));
        
        assert (zre_msg_sequence (self) == 123);
        zre_msg_destroy (&self);
    }

    zctx_destroy (&ctx);
    //  @end

    printf ("OK\n");
    return 0;
}
