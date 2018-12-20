using System.Collections;
using System.Collections.Generic;
using UnityEngine;

using Matryx;

public class TournamentContainer : MonoBehaviour {

    MatryxTournament tournament;
	
    public void SetTournament(MatryxTournament tournament)
    {
        this.tournament = tournament;
    }

    public MatryxTournament GetTournament()
    {
        return tournament;
    }
}
