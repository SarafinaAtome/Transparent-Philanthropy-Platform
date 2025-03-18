import { describe, expect, it } from "vitest";
import { Cl } from "@stacks/transactions";

const accounts = simnet.getAccounts();
const donor1 = accounts.get("wallet_1")!;
const donor2 = accounts.get("wallet_2")!;
const cause = "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM";

describe("Transparent Philanthropy Platform", () => {
    it("successfully makes a donation", () => {
        const donationAmount = 1000;
        
        const makeDonation = simnet.callPublicFn(
            "tpp",
            "make-donation",
            [
                Cl.uint(donationAmount),
                Cl.principal(cause)
            ],
            donor1
        );
        
        expect(makeDonation.result).toBeOk(Cl.uint(0));
    });

    it("correctly tracks donation count per donor", () => {
        // Make two donations from donor1
        simnet.callPublicFn(
            "tpp",
            "make-donation",
            [Cl.uint(1000), Cl.principal(cause)],
            donor1
        );
        
        simnet.callPublicFn(
            "tpp",
            "make-donation",
            [Cl.uint(2000), Cl.principal(cause)],
            donor1
        );

        const getDonorCount = simnet.callReadOnlyFn(
            "tpp",
            "get-donor-donation-count",
            [Cl.principal(donor1)],
            donor1
        );

        expect(getDonorCount.result).toBeOk(Cl.uint(2));
    });


  });