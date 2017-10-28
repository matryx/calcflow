using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Matryx_Tournament
{
    public Matryx_Tournament(EthereumAddress address)
    {
        this.address = address;
    }
    public Matryx_Tournament(string title, string descriptionAddress, long bounty)
    {
        this.title = title;
        this.descriptionAddress = descriptionAddress;
        this.bounty = bounty;
    }

    public EthereumAddress address;
    public string title;
    public string descriptionAddress;
    public string description;
    public long? bounty;
    //public DateTime startTime;
    //public DateTime roundDuration;
    //public int round;
    //public DateTime reviewDuration;
    //public List<EthereumAddress> submissions;
    //public EthereumAddress updateAgentAddress;

    // Getters (all assume address is non-null and correct)
    public string getTitle()
    {
        if(title != null)
        {
            return title;
        }
        
        //TODO: Implement
        return "tournament";
        
    }

    public string getDescription()
    {
        if(description != null)
        {
            return description;
        }

        //TODO: Implement
        return "description from " + descriptionAddress;
    }

    public long getBounty()
    {
        if (bounty != null)
        {
            return bounty.Value;
        }

        //TODO: Implement
        return -1;
    }
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
