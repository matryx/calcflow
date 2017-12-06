using UnityEngine;

using System;
using System.Collections.Generic;

namespace MatryxJsonRpc
{

    public class Submission
    {

        public string title;
        public string body;

        public string author;

        public string address;
        public string tournamentAddress;

        public string references;
        public string contributors;

        public List<string> contributorsList()
        {
            return new List<string>(this.contributors.Split('\n'));
        }
        public List<string> referencesList()
        {
            return new List<string>(this.references.Split('\n'));
        }

        public void contributorsList(List<string> contributors)
        {
            this.contributors = string.Join("\n", contributors.ToArray());
        }
        public void referencesList(List<string> references)
        {
            this.references = string.Join("\n", references.ToArray());
        }

    }

}

