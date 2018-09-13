;;; forge-gitlab.el --- gitlab support            -*- lexical-binding: t -*-

;; Copyright (C) 2018  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; Magit is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see http://www.gnu.org/licenses.

;;; Code:

(require 'glab)

(require 'forge)
(require 'forge-issue)
(require 'forge-pullreq)

;;; Variables

(defvar forge-gitlab-token-scopes '(api)
  "The Gitlab API scopes needed by Magit.

`api' is the only required scope.  It gives read and write access
to everything.  The Gitlab API provides more fine-grained scopes
for read-only access, but when any write access is required, then
it is all or nothing.")

;;; Repositories

;; The `forge-gitlab-repository' class is defined in ghub-repo.el

;;; Pull

(cl-defmethod forge--pull ((repo forge-gitlab-repository))
  (forge--pull-issues repo)
  (forge--pull-pullreqs repo))

;;;; Issues

(cl-defmethod forge--pull-issues ((repo forge-gitlab-repository))
  (emacsql-with-transaction (forge-db)
    (dolist (i (forge--fetch-issues repo))
      (let-alist i
        (let* ((issue-id (forge--object-id 'forge-issue repo .iid))
               (issue
                (forge-issue
                 :id        issue-id
                 :repository   (oref repo id)
                 :number    .iid
                 :state     (intern .state)
                 :author    .author.username
                 :title     .title
                 :created   .created_at
                 :updated   .updated_at
                 :closed    .closed_at
                 :locked-p  .discussion_locked
                 :milestone .milestone.iid
                 :body      (forge--sanitize-string .description))))
          (closql-insert (forge-db) issue t)
          (dolist (c .notes)
            (let-alist c
              (let ((post
                     (forge-issue-post
                      :id      (forge--object-id issue-id .id)
                      :issue   issue-id
                      :number  .id
                      :author  .author.username
                      :created .created_at
                      :updated .updated_at
                      :body    (forge--sanitize-string .body))))
                (closql-insert (forge-db) post t)))))))))

(cl-defmethod forge--fetch-issues ((repo forge-gitlab-repository))
  (with-slots (owner name) repo
    (mapcar
     (lambda (issue)
       (let-alist issue
         (magit-msg "Pulling issue %s/%s#%s..." owner name .iid)
         ;; NOTE Gitlab has no decent endpoint to get posts on
         ;; an issue.  The list we get here includes all kinds
         ;; of events.  There is a `type' field, but its value
         ;; is always nil.  For now we just display all events.
         ;; We probably will have to reverse engineer and come
         ;; up with some heuristics; I am not holding my breath:
         ;; https://gitlab.com/gitlab-org/gitlab-ce/issues/20901
         (setf (alist-get 'notes issue)
               (forge--glab-get
                repo (format "/projects/%s/issues/%s/notes" .project_id .iid)
                '((per_page . 100)) :unpaginate t)))
       issue)
     (forge--glab-get repo
                      (format "/projects/%s%%2F%s/issues" owner name)
                      '((per_page . 100)) :unpaginate nil))))

;;;; Pullreqs

(cl-defmethod forge--pull-pullreqs ((repo forge-gitlab-repository))
  (emacsql-with-transaction (forge-db)
    (dolist (p (forge--fetch-pullreqs repo))
      (let-alist p
        (let* ((pullreq-id (forge--object-id 'forge-pullreq repo .iid))
               (pullreq
                (forge-pullreq
                 :id           pullreq-id
                 :repository   (oref repo id)
                 :number       .iid
                 :state        (intern .state)
                 :author       .author.username
                 :title        .title
                 :created      .created_at
                 :updated      .updated_at
                 ;; NOTE .merged_at and .closed_at may both be nil
                 ;; even though the pullreq is merged or otherwise
                 ;; closed.  In such cases use 1, so that these
                 ;; variables at least can serve as booleans.
                 :closed       (or .closed_at
                                   (and (member .state '("closed" "merged")) 1))
                 :merged       (or .merged_at
                                   (and (equal .state "merged") 1))
                 :locked-p     .discussion_locked
                 :editable-p   .allow_maintainer_to_push
                 :cross-repo-p (not (equal .source_project_id
                                           .target_project_id))
                 :base-ref     .target_branch
                 :base-repo    .target_project.path_with_namespace
                 :head-ref     .source_branch
                 :head-user    .source_project.owner.username
                 :head-repo    .source_project.path_with_namespace
                 :milestone    .milestone.iid
                 :body         (forge--sanitize-string .description))))
          (closql-insert (forge-db) pullreq t)
          (dolist (c .notes)
            (let-alist c
              (let ((post
                     (forge-pullreq-post
                      :id      (forge--object-id pullreq-id .id)
                      :pullreq pullreq-id
                      :number  .id
                      :author  .author.username
                      :created .created_at
                      :updated .updated_at
                      :body    (forge--sanitize-string .body))))
                (closql-insert (forge-db) post t)))))))))

(cl-defmethod forge--fetch-pullreqs ((repo forge-gitlab-repository))
  (with-slots (owner name) repo
    (let (target-project)
      (mapcar
       (lambda (pullreq)
         ;; NOTE When fetching multiple pullreqs at once, then the
         ;; pullreqs lack some data, so we have to fetch each one
         ;; individually.
         (let ((number (cdr (assq 'iid pullreq))))
           (magit-msg "Pulling pullreq %s/%s#%s..." owner name number)
           (setq pullreq
                 (forge--glab-get
                  repo (format "/projects/%s%%2F%s/merge_requests/%s"
                              owner name number))))
         (let-alist pullreq
           (when .source_project_id
             ;; If the fork no longer exists, then this is nil.
             ;; This will lead to difficulties later on but there
             ;; is nothing we can do about it.
             (setf (alist-get 'source_project pullreq)
                   (forge--glab-get
                    repo (format "/projects/%s" .source_project_id))))
           (setf (alist-get 'target_project pullreq)
                 (or target-project
                     (setq target-project
                           (forge--glab-get
                            repo (format "/projects/%s" .target_project_id)))))
           (setf (alist-get 'notes pullreq)
                 (forge--glab-get
                  repo (format "/projects/%s/merge_requests/%s/notes"
                              .target_project_id .iid)
                  '((per_page . 100)) :unpaginate t)))
         pullreq)
       (forge--glab-get repo (format "/projects/%s%%2F%s/merge_requests"
                                    owner name)
                        '((per_page . 100)) :unpaginate t)))))

;;;; Notifications

(cl-defmethod forge--pull-notifications
  ((_class (subclass forge-gitlab-repository)) _githost &optional _repo)
  ;; NOTE The closest to notifications that Gitlab provides are
  ;; "events" as described at https://docs.gitlab.com/ee/api/events.html.
  ;; This allows us to see the last N events that took place, but that
  ;; is not good enough - we are mostly interested in events we
  ;; haven't looked at yet.  Gitlab doesn't make a distinction between
  ;; unread and read events, so this is rather useless and we don't
  ;; use it for the time being.
  )

;;; Utilities

(cl-defun forge--glab-get (repo resource
                               &optional params
                               &key query payload headers
                               silent unpaginate noerror reader
                               callback errorback)
  (glab-get resource params
            :host (oref repo apihost)
            :auth 'forge
            :query query :payload payload :headers headers
            :silent silent :unpaginate unpaginate
            :noerror noerror :reader reader
            :callback callback :errorback errorback))

;;; _
(provide 'forge-gitlab)
;;; forge-gitlab.el ends here
