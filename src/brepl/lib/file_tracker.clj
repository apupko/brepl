(ns brepl.lib.file-tracker
  "Track file changes across tool uses for automatic REPL reloading.
   Uses mtime (modification time) for O(n) stat calls instead of reading files."
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(def ^:private source-file-re #"\.(clj|cljs|cljc|edn)$")

(defn- git-clojure-files
  "List Clojure files known to git: tracked plus untracked-but-not-ignored.

   Returns nil when dir is not a git work tree or git is unavailable, so
   callers fall back to walking the filesystem.

   Preferred over a filesystem walk because git prunes ignored directories
   during traversal instead of after it. On a project carrying build output
   that is the difference between visiting every artifact and visiting none:
   glob descends into target/ and .cpcache/ and only then discards what it
   found, so the disk read has already happened."
  [dir]
  (try
    (let [{:keys [exit out]} (process/sh {:dir dir :out :string :err :string}
                                         "git" "ls-files" "-co" "--exclude-standard" "-z")]
      (when (zero? exit)
        ;; git reports paths relative to dir; the walk fallback yields
        ;; dir-prefixed paths, so join here to keep one contract for callers.
        (->> (str/split out #"\x00")
             (filter #(re-find source-file-re %))
             (mapv #(str (fs/path dir %))))))
    (catch Exception _ nil)))

(defn- walk-clojure-files
  "Filesystem fallback for non-git directories. Excludes common build and
   tooling directories, but only after the walk has already visited them —
   this is why git-clojure-files is tried first."
  [dir]
  (let [excluded-dirs #{".git" "node_modules" "target" ".cpcache" ".clj-kondo" ".lsp"}]
    (->> (fs/glob dir "**/*.{clj,cljs,cljc,edn}")
         (remove (fn [path]
                   (some #(str/includes? (str path) (str "/" % "/"))
                         excluded-dirs)))
         (map str)
         vec)))

(defn find-clojure-files
  "Find all Clojure files in the given directory (default: cwd).
   Excludes common non-source directories."
  ([] (find-clojure-files "."))
  ([dir]
   (->> (or (git-clojure-files dir)
            (walk-clojure-files dir))
        (filter #(.isFile (io/file %)))
        vec)))

(defn file-mtime
  "Get file modification time in millis. Returns nil if file doesn't exist."
  [path]
  (when (fs/exists? path)
    (fs/file-time->millis (fs/last-modified-time path))))

(defn mtime-all-files
  "Get mtime for all given files. Returns map of path -> mtime."
  [file-paths]
  (->> file-paths
       (map (fn [path] [path (file-mtime path)]))
       (into {})))

(defn state-file
  "Get the state file path for a session."
  [session-id]
  (let [dir (io/file "/tmp" (str "brepl-tracker-" session-id))]
    (.mkdirs dir)
    (io/file dir "mtimes.edn")))

(defn save-mtimes
  "Save file mtimes for a session."
  [session-id mtimes]
  (spit (state-file session-id) (pr-str mtimes)))

(defn load-mtimes
  "Load saved file mtimes for a session."
  [session-id]
  (let [f (state-file session-id)]
    (when (.exists f)
      (edn/read-string (slurp f)))))

(def ^:private file-list-cache (atom nil))

(defn- cached-clojure-files
  "find-clojure-files memoised for the lifetime of this process.

   A hook process calls detect-changes and then snapshot!, which would
   otherwise scan the project twice for a file set that cannot change in
   between. Only the listing is cached — mtimes are always read fresh, so a
   file rewritten by the bracket fixer is still picked up by the snapshot."
  []
  (or @file-list-cache
      (reset! file-list-cache (find-clojure-files))))

(defn detect-changes
  "Compare current mtimes with saved mtimes.
   Returns vector of changed file paths (modified or new files)."
  [session-id]
  (let [saved (load-mtimes session-id)]
    (when saved
      (let [current-files (cached-clojure-files)
            current (mtime-all-files current-files)]
        (->> current
             (filter (fn [[path mtime]]
                       (let [old-mtime (get saved path)]
                         ;; Changed if: new file (no old-mtime) or mtime differs
                         (or (nil? old-mtime)
                             (not= old-mtime mtime)))))
             (map first)
             vec)))))

(defn snapshot!
  "Take a snapshot of all Clojure files in the project.
   Returns the number of files tracked."
  [session-id]
  (let [files (cached-clojure-files)
        mtimes (mtime-all-files files)]
    (save-mtimes session-id mtimes)
    (count files)))

(defn cleanup-state
  "Clean up tracking state for a session."
  [session-id]
  (let [dir (io/file "/tmp" (str "brepl-tracker-" session-id))]
    (when (.exists dir)
      (doseq [file (.listFiles dir)]
        (.delete file))
      (.delete dir))))
