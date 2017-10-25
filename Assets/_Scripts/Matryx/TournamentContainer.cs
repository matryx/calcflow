using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public struct Matryx_Tournament
{
    public Matryx_Tournament(string name, string descriptionAddress, long bounty)
    {
        this.name = name;
        this.descriptionAddress = descriptionAddress;
        this.bounty = bounty;
    }
    public string name;
    public string descriptionAddress;
    public long bounty;
    //public DateTime startTime;
    //public DateTime roundDuration;
    //public int round;
    //public DateTime reviewDuration;
    //public List<EthereumAddress> submissions;
    //public EthereumAddress updateAgentAddress;
}

public struct EthereumAddress
{
        byte[] _address;
}

public class TournamentContainer : MonoBehaviour {

    Matryx_Tournament tournament;
	
    public void SetTournament(Matryx_Tournament tournament)
    {
        this.tournament = tournament;
    }

    public Matryx_Tournament GetTournament()
    {
        return tournament;
    }
}
