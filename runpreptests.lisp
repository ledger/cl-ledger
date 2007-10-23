(push "src/" asdf:*central-registry*)
(asdf:operate 'asdf:load-op :cambl)

(push "test/" asdf:*central-registry*)
(asdf:operate 'asdf:load-op :cambl-test)
